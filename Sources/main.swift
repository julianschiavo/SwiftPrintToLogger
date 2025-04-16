// SwiftSyntax-based script to replace print calls with os.Logger usage
// Usage: swift run ReplacePrintLogger --root <directory> --module <ModuleName> [--dry-run]
//
// This script:
// - Scans Swift files in the provided directory.
// - Only processes files that contain exactly one top-level class or struct.
// - Inserts "import os.log" if not present.
// - Injects "let logger = Logger(subsystem: \"<ModuleName>\", category: \"<TypeName>\")" 
//   at the end of the type's members (if not already there).
// - Replaces print(...) calls with logger.info(...) or logger.error(...), deciding the method
//   based on whether the call occurs within a catch block or the message contains error-indicating keywords.
// - Overwrites the original file, but in --dry-run mode only modifies the first 3 matching files.

import Foundation
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftParser

// MARK: - Command-line Options Parsing

struct Options {
    var rootPath: String
    var moduleName: String
    var dryRun: Bool
}

func parseOptions() -> Options? {
    var rootPath: String?
    var moduleName: String?
    var dryRun = false
    var i = 1
    let args = CommandLine.arguments
    while i < args.count {
        let arg = args[i]
        switch arg {
        case "--dry-run", "-n":
            dryRun = true
        case "--module":
            i += 1
            if i < args.count {
                moduleName = args[i]
            } else {
                fputs("Error: --module requires a value\n", stderr)
                return nil
            }
        case "--root":
            i += 1
            if i < args.count {
                rootPath = args[i]
            } else {
                fputs("Error: --root requires a directory path\n", stderr)
                return nil
            }
        default:
            // Treat positional arguments as root path or module name if not provided via flags.
            if arg.hasPrefix("-") {
                fputs("Unknown option: \(arg)\n", stderr)
                return nil
            }
            if rootPath == nil {
                rootPath = arg
            } else if moduleName == nil {
                moduleName = arg
            } else {
                fputs("Too many arguments: \(arg)\nUsage: script [--dry-run] --module <Name> --root <Directory>\n", stderr)
                return nil
            }
        }
        i += 1
    }
    guard let root = rootPath, let module = moduleName else {
        fputs("Usage: script [--dry-run] --module <Name> --root <Directory>\n", stderr)
        return nil
    }
    return Options(rootPath: root, moduleName: module, dryRun: dryRun)
}

// MARK: - Trivia Helper

extension Trivia {
    func containsNewlines(count: Int) -> Bool {
        var newlineCount = 0
        for piece in self {
            switch piece {
            case .newlines(let n), .carriageReturnLineFeeds(let n):
                newlineCount += n
            case .carriageReturns(let n):
                 newlineCount += n // Count CR as newline for spacing check
            default:
                continue
            }
            if newlineCount >= count {
                return true
            }
        }
        return false
    }
    
    func removingNewlinesBeforeFirstCommentOrCode() -> Trivia {
        // Find the first trivia piece that isn't just whitespace/newlines
        guard let firstNonWhitespaceIndex = firstIndex(where: { !$0.isSpaceOrNewline }) else {
            // If only whitespace, return empty trivia
            return Trivia()
        }
        // Return the trivia starting from that piece
        return Trivia(pieces: Array(self[firstNonWhitespaceIndex...]))
    }

    var isSpaceOrNewline: Bool {
        pieces.allSatisfy { $0.isSpaceOrNewline }
    }
}

extension TriviaPiece {
    var isSpaceOrNewline: Bool {
        switch self {
        case .spaces, .tabs, .newlines, .carriageReturns, .carriageReturnLineFeeds:
            return true
        default:
            return false
        }
    }
}

// MARK: - SwiftSyntax Rewriter

class PrintToLoggerRewriter: SyntaxRewriter {
    let moduleName: String
    private var currentTypeName: String?
    private var currentTypeKind: SyntaxKind? // Added to track type kind
    private var isCurrentTypeGeneric: Bool = false // Track genericity
    var pendingFileLevelLoggers: [String: String] = [:] // Track loggers for generic types [TypeName: DeclString]
    // Add a set to track types that actually contain print calls
    var typesContainingPrint: Set<String> = []

    init(moduleName: String) {
        self.moduleName = moduleName
    }

    // Helper to find enclosing type and if inside a closure
    private func findEnclosingContext(startNode: Syntax) -> (typeName: String?, typeKind: SyntaxKind?, isInClosure: Bool, isGeneric: Bool) {
        var current: Syntax? = startNode.parent
        var enclosingTypeName: String? = nil
        var enclosingTypeKind: SyntaxKind? = nil
        var isInClosure = false
        var isGeneric = false // Added to track genericity

        while let node = current {
            // Check for closures first
            if !isInClosure && node.is(ClosureExprSyntax.self) {
                isInClosure = true
                // Don't break here, continue searching upwards for the type
            }

            // Check for type declarations
            if enclosingTypeName == nil { // Only find the *innermost* enclosing type
                if let classDecl = node.as(ClassDeclSyntax.self) {
                    enclosingTypeName = classDecl.name.text
                    enclosingTypeKind = .classDecl
                    isGeneric = classDecl.genericParameterClause != nil // Check genericity
                    // Don't break, continue checking for closures further up if needed
                } else if let structDecl = node.as(StructDeclSyntax.self) {
                    enclosingTypeName = structDecl.name.text
                    enclosingTypeKind = .structDecl
                    isGeneric = structDecl.genericParameterClause != nil // Check genericity
                    // Don't break
                } else if let enumDecl = node.as(EnumDeclSyntax.self) {
                    enclosingTypeName = enumDecl.name.text
                    enclosingTypeKind = .enumDecl
                    isGeneric = enumDecl.genericParameterClause != nil // Check genericity
                    // Don't break
                } else if let actorDecl = node.as(ActorDeclSyntax.self) {
                    enclosingTypeName = actorDecl.name.text
                    enclosingTypeKind = .actorDecl
                    isGeneric = actorDecl.genericParameterClause != nil // Check genericity (Actors can be generic)
                    // Don't break
                }
            }

            // If we've found the type AND determined if we are in a closure, we can potentially stop early,
            // but iterating all the way up is safer to catch all closure contexts.
            // if enclosingTypeName != nil && isInClosure { break }

            current = node.parent
        }
        // Return ONLY the directly found context from walking the tree.
        return (typeName: enclosingTypeName, typeKind: enclosingTypeKind, isInClosure: isInClosure, isGeneric: isGeneric)
    }

    override func visit(_ node: FunctionCallExprSyntax) -> ExprSyntax {
        guard let called = node.calledExpression.as(DeclReferenceExprSyntax.self),
              called.baseName.text == "print" else {
            return super.visit(node)
        }

        let context = findEnclosingContext(startNode: Syntax(node))

        // Only replace print if we are reasonably sure a 'logger' is in scope
        // This now depends on the context (generic type -> file logger, non-generic -> member logger)
        let enclosingTypeFound = context.typeKind != nil && context.typeName != nil
        // Let's also check if the current node is actually *within* the body of the enclosing type
        // (This helps filter out print calls in extensions defined in the same file but outside the primary type)
        // This might be overly complex; let's rely on the logger injection logic for now.

        // Record the type name if we are replacing a print call within a type context
        if let typeName = context.typeName, enclosingTypeFound {
            typesContainingPrint.insert(typeName)
            // print(">>> Found print in \\(typeName)") // DEBUG
        }

        let isInsideTaskClosure = context.isInClosure // Re-evaluate if this is the best way to detect Task{}
        // GUARD condition adjustment: Allow replacement if type context OR task closure detected,
        // but the logger *injection* will depend on typesContainingPrint.
        guard enclosingTypeFound || isInsideTaskClosure else {
            // print("Skipping print replacement outside of a recognized type or Task/closure context.") // DEBUG
            return super.visit(node) // Don't replace if not inside a processed type or detected closure
        }

        // If typeName wasn't found by walking up, but we are in a closure, we might need it for struct/enum prefix.
        // However, assuming Task/closures are primarily used in classes/actors where `self.` is needed anyway.
        // let containingTypeName = context.typeName // Might be nil if only isInsideTaskClosure is true // Remove unused variable

        // Determine log level based on context and content
        var logLevel = "info" // Default level

        // Check if inside a catch block
        var parent = node.parent
        var inCatchBlock = false
        while let current = parent {
            if current.kind == .catchClause {
                logLevel = "error" // Default to error in catch blocks
                inCatchBlock = true
                break
            }
            parent = current.parent
        }

        // Define keywords for different levels (lowercase)
        let faultKeywords = ["critical", "fatal", "💣", "💥", "[critical]", "(critical)", "[fatal]", "(fatal)"]
        let errorKeywords = ["error", "fail", "failed", "exception", "err", "assert", "assertion", "🔥", "[error]", "(error)"]
        let warningKeywords = ["warning", "warn", "deprecated", "caution", "attn", "attention", "⚠️", "[warning]", "(warning)"]
        let debugKeywords = ["debug", "dbg", "trace", "verbose", "[debug]", "(debug)", "[trace]", "(trace)", "enter", "entering", "exit", "exiting", "->", "<-", "🐛"]
        // Note: Info/Notice keywords aren't strictly needed as it's the default, but can be used for overrides
        // let infoKeywords = ["notice", "note", "[info]", "(info)", "[notice]", "(notice)"]

        var detectedLevel: String? = nil

        // Build the log message string and simultaneously check for keywords
        var logMessageCode = "\""
        for (index, arg) in node.arguments.enumerated() {
            if index > 0 { logMessageCode += " " }
            
            var argumentText = "" // For keyword checking (uses raw expression text)
            // Remove unused variable
            // var containsInterpolation = false

            // Determine the expression being printed/interpolated
            let expressionToProcess = arg.expression
            // Remove unused variable
            // var requiresSelfPrefixingCheck = false // Flag if this arg might need 'self.' prefix

            if let stringLiteral = expressionToProcess.as(StringLiteralExprSyntax.self) {
                // Handle string literals segment by segment
                for segment in stringLiteral.segments {
                    if let textSegment = segment.as(StringSegmentSyntax.self) {
                        // Plain text segment
                        let text = textSegment.content.text
                        argumentText += text
                        var escaped = text
                        escaped = escaped.replacingOccurrences(of: "\\", with: "\\\\")
                        escaped = escaped.replacingOccurrences(of: "\"", with: "\\\"")
                        escaped = escaped.replacingOccurrences(of: "\n", with: "\\n")
                        escaped = escaped.replacingOccurrences(of: "\t", with: "\\t")
                        logMessageCode += escaped
                    } else if let exprSegment = segment.as(ExpressionSegmentSyntax.self),
                              let interpolatedExpr = exprSegment.expressions.first?.expression {
                        // Interpolated expression segment: \\(...)
                        // Remove unused variable
                        // containsInterpolation = true
                        let rawExprText = interpolatedExpr.description
                        var exprTextForLog = rawExprText // Text to use in the logger string

                        // --- Check if 'self.' should be prepended ---
                        var needsSelfPrefix = false // Default: No prefix

                        // Condition 1: Are we in a context where 'self.' might be needed? (Class/Actor, non-generic)
                        let selfPrefixPotentiallyNeeded = !context.isGeneric && (context.typeKind == .classDecl || context.typeKind == .actorDecl)

                        if selfPrefixPotentiallyNeeded {
                            // Condition 2: Is the expression potentially referring to an instance member?
                            var isPotentiallyInstanceMemberAccess = false
                            var identifierToCheckForLocalScope: TokenSyntax? = nil // Track identifier if simple

                            if let simpleIdentifier = interpolatedExpr.as(DeclReferenceExprSyntax.self) {
                                // It's 'foo'. Assume it MIGHT be an instance member.
                                isPotentiallyInstanceMemberAccess = true
                                identifierToCheckForLocalScope = simpleIdentifier.baseName
                            } else if let memberAccess = interpolatedExpr.as(MemberAccessExprSyntax.self) {
                                if memberAccess.base == nil {
                                    // Implicit member '.foo' -> YES
                                    isPotentiallyInstanceMemberAccess = true
                                }
                                // else: Explicit member access like 'a.b' or 'self.b'.
                                // We don't automatically prefix these chains.
                                // If 'a' is an instance var, it might need 'self.a.b', but handling that
                                // reliably requires deeper analysis. Focus on direct/implicit access.
                            } // else: Literals, function calls, etc. -> No

                            // Condition 3: Check if the identifier (if found) is a local parameter.
                            // START REPLACEMENT
                            var isDeclaredLocally = false
                            if let ident = identifierToCheckForLocalScope {
                                var currentScopeNode: Syntax? = Syntax(node) // Start from the print call

                                while let scopeNode = currentScopeNode {
                                    // 1. Check function/method/initializer/subscript parameters
                                    if let funcDecl = scopeNode.as(FunctionDeclSyntax.self) {
                                        if funcDecl.signature.parameterClause.parameters.contains(where: { $0.firstName.text == ident.text || $0.secondName?.text == ident.text }) {
                                            isDeclaredLocally = true; break
                                        }
                                    } else if let initDecl = scopeNode.as(InitializerDeclSyntax.self) {
                                        if initDecl.signature.parameterClause.parameters.contains(where: { $0.firstName.text == ident.text || $0.secondName?.text == ident.text }) {
                                            isDeclaredLocally = true; break
                                        }
                                    } else if let subscriptDecl = scopeNode.as(SubscriptDeclSyntax.self) {
                                        if subscriptDecl.parameterClause.parameters.contains(where: { $0.firstName.text == ident.text || $0.secondName?.text == ident.text }) {
                                            isDeclaredLocally = true; break
                                        }
                                    }
                                    // 2. Check closure parameters
                                    else if let closureExpr = scopeNode.as(ClosureExprSyntax.self) {
                                        // Check formal parameters
                                        if let params = closureExpr.signature?.parameterClause?.as(ClosureParameterClauseSyntax.self) {
                                            if params.parameters.contains(where: { param in param.firstName.text == ident.text }) {
                                                isDeclaredLocally = true; break // Found formal closure param
                                            }
                                        }
                                        // Check shorthand parameters
                                        else if closureExpr.signature?.parameterClause?.is(ClosureShorthandParameterListSyntax.self) ?? false {
                                             if ident.text.hasPrefix("$") && Int(ident.text.dropFirst()) != nil {
                                                 isDeclaredLocally = true; break // Found shorthand closure param
                                             }
                                        }
                                    }
                                    // 3. Check 'catch' block variable
                                    else if let catchClause = scopeNode.as(CatchClauseSyntax.self) {
                                        // Check for 'catch let errorName' or implicit 'error'
                                        let catchItemList = catchClause.catchItems
                                        // Remove unused variable
                                        // var foundInCatch = false
                                        var catchErrorName: String? = nil // nil means implicit 'error'
                                        if catchItemList.isEmpty || (catchItemList.count == 1 && catchItemList.first?.pattern == nil) {
                                            // Simple 'catch' -> implicit 'error'
                                            catchErrorName = nil
                                        } else {
                                             for item in catchItemList {
                                                 if let pattern = item.pattern {
                                                     if let bindingPattern = pattern.as(ValueBindingPatternSyntax.self),
                                                        let identifierPattern = bindingPattern.pattern.as(IdentifierPatternSyntax.self) {
                                                          catchErrorName = identifierPattern.identifier.text; break
                                                     } else if let identifierPattern = pattern.as(IdentifierPatternSyntax.self) {
                                                          // Handles 'case let x' inside catch? Or direct identifier pattern?
                                                          catchErrorName = identifierPattern.identifier.text; break
                                                     } // Add more complex pattern checks if needed (e.g., tuple bindings)
                                                 }
                                             }
                                             // If loop finishes without binding a simple name, assume implicit 'error' if items exist but didn't match simple binding
                                             // This might need refinement based on complex catch patterns.
                                        }
                                         if (catchErrorName == nil && ident.text == "error") || (catchErrorName != nil && ident.text == catchErrorName) {
                                              isDeclaredLocally = true; break
                                         }
                                    }
                                    // 4. Check 'for' loop variable
                                    else if let forStmt = scopeNode.as(ForStmtSyntax.self) {
                                        // Is the print call *inside* this loop's body?
                                        if node.position >= forStmt.body.position && node.endPosition <= forStmt.body.endPosition {
                                            // Does the identifier match the loop variable pattern?
                                            if let idPattern = forStmt.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                 isDeclaredLocally = true; break
                                            }
                                             // TODO: Handle tuple patterns like 'for (key, value) in dict'
                                             // For tuple patterns, check if any identifier matches
                                             if let tuplePattern = forStmt.pattern.as(TuplePatternSyntax.self) {
                                                 for element in tuplePattern.elements {
                                                     if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                         isDeclaredLocally = true; break
                                                     }
                                                     // Recursively check nested tuples? For now, direct identifiers.
                                                 }
                                                 if isDeclaredLocally { break }
                                             }
                                        }
                                    }
                                    // 5. Check 'guard let/var' bindings *if the print call is AFTER the guard*
                                    else if let guardStmt = scopeNode.as(GuardStmtSyntax.self) {
                                        // Check if print call's position is after the guard statement's end position
                                        if node.position > guardStmt.endPosition {
                                            for condition in guardStmt.conditions {
                                                 if let optBinding = condition.condition.as(OptionalBindingConditionSyntax.self) {
                                                      if let idPattern = optBinding.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                          isDeclaredLocally = true; break // Found guard let/var binding
                                                      }
                                                       // TODO: Handle tuple patterns in guard let
                                                        if let tuplePattern = optBinding.pattern.as(TuplePatternSyntax.self) {
                                                             for element in tuplePattern.elements {
                                                                 if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                                     isDeclaredLocally = true; break
                                                                 }
                                                             }
                                                             if isDeclaredLocally { break }
                                                        }
                                                 }
                                             }
                                            if isDeclaredLocally { break }
                                        }
                                    }
                                    // 6. Check 'if let/var' bindings *if the print call is INSIDE the relevant block*
                                    else if let ifStmt = scopeNode.as(IfExprSyntax.self) { // CORRECTED TYPE
                                        // Check conditions for the 'if' part
                                        var foundInIfCondition = false
                                        for condition in ifStmt.conditions {
                                             if let optBinding = condition.condition.as(OptionalBindingConditionSyntax.self) {
                                                  if let idPattern = optBinding.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                      // Is the print call inside the 'if' body?
                                                       if node.position >= ifStmt.body.position && node.endPosition <= ifStmt.body.endPosition {
                                                          isDeclaredLocally = true; foundInIfCondition = true; break
                                                      }
                                                  }
                                                   // TODO: Handle tuple patterns
                                                    if let tuplePattern = optBinding.pattern.as(TuplePatternSyntax.self) {
                                                         for element in tuplePattern.elements {
                                                             if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                                  if node.position >= ifStmt.body.position && node.endPosition <= ifStmt.body.endPosition {
                                                                      isDeclaredLocally = true; foundInIfCondition = true; break
                                                                  }
                                                             }
                                                         }
                                                         if foundInIfCondition { break }
                                                    }
                                             }
                                         }
                                        if foundInIfCondition { break }

                                        // Check 'else if' clauses recursively upwards (handled by the loop continuing)
                                        // or check if inside the 'else' block.
                                         if let elseBody = ifStmt.elseBody {
                                             if let _ = elseBody.as(CodeBlockSyntax.self) { // Direct 'else' block
                                                 // If node is inside this 'else', the 'if' bindings don't apply to it.
                                                 // The loop continues upwards to check scopes outside the 'if'.
                                             }
                                             // If it's an 'else if', the loop continues upwards and will hit the nested 'ifStmt'.
                                         }
                                    }

                                    // 7. Check local 'let'/'var' declarations within the current code block scope
                                    // Only check if scopeNode IS a block container (func body, closure, if/else, do, etc.)
                                    // Need a way to get the direct CodeBlockSyntax if applicable.
                                    var currentCodeBlock: CodeBlockSyntax? = nil
                                    if let funcDecl = scopeNode.as(FunctionDeclSyntax.self) { currentCodeBlock = funcDecl.body }
                                    else if let initDecl = scopeNode.as(InitializerDeclSyntax.self) { currentCodeBlock = initDecl.body }
                                    else if let closureExpr = scopeNode.as(ClosureExprSyntax.self) { currentCodeBlock = CodeBlockSyntax(statements: closureExpr.statements) } // Synthesize block
                                    else if let ifStmt = scopeNode.as(IfExprSyntax.self) { // CORRECTED TYPE
                                        // Is print inside the 'if' body?
                                        // IfExprSyntax uses body: CodeBlockSyntax
                                        if node.position >= ifStmt.body.position && node.endPosition <= ifStmt.body.endPosition { currentCodeBlock = ifStmt.body }
                                        // IfExprSyntax uses elseBody: Syntax? which can be CodeBlockSyntax or IfExprSyntax
                                        else if let elseSyntax = ifStmt.elseBody {
                                             if let elseBody = elseSyntax.as(CodeBlockSyntax.self), node.position >= elseBody.position && node.endPosition <= elseBody.endPosition { currentCodeBlock = elseBody }
                                             // else-if case is handled by the next iteration of the while loop
                                        }
                                    }
                                    else if let forStmt = scopeNode.as(ForStmtSyntax.self) { currentCodeBlock = forStmt.body }
                                    else if let whileStmt = scopeNode.as(WhileStmtSyntax.self) { currentCodeBlock = whileStmt.body }
                                    else if let repeatStmt = scopeNode.as(RepeatStmtSyntax.self) { currentCodeBlock = repeatStmt.body }
                                    else if let doStmt = scopeNode.as(DoStmtSyntax.self) { currentCodeBlock = doStmt.body }
                                    // Check accessors in Subscript or Properties? -> Complex, maybe defer
                                    else if let accessorDecl = scopeNode.as(AccessorDeclSyntax.self) { currentCodeBlock = accessorDecl.body }
                                    // Catch clause body check:
                                    else if let catchClause = scopeNode.as(CatchClauseSyntax.self) { currentCodeBlock = catchClause.body }


                                    if let codeBlock = currentCodeBlock {
                                        // Iterate statements *within this block* that appear *before* the print call
                                        for stmtItem in codeBlock.statements {
                                             // Stop searching this block if we go past the print call's position
                                             if stmtItem.position >= node.position { // Use >= ? Or > node.endPosition? Let's try >= node.position
                                                 break
                                             }
                                             if let varDecl = stmtItem.item.as(VariableDeclSyntax.self) {
                                                 for binding in varDecl.bindings {
                                                     if let idPattern = binding.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                         isDeclaredLocally = true; break // Found local var/let before print call
                                                     }
                                                      // TODO: Handle tuple patterns
                                                     if let tuplePattern = binding.pattern.as(TuplePatternSyntax.self) {
                                                         for element in tuplePattern.elements {
                                                              if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                                  isDeclaredLocally = true; break
                                                              }
                                                         }
                                                     }
                                                 }
                                             }
                                             if isDeclaredLocally { break } // Break inner statement loop
                                        }
                                         if isDeclaredLocally { break } // Break outer while loop
                                    }
                                    
                                    // Stop searching upwards if we hit a type declaration (class, struct, enum, actor)
                                    // because parameters/local vars outside the type are not relevant here.
                                    if scopeNode.is(ClassDeclSyntax.self) || scopeNode.is(StructDeclSyntax.self) || scopeNode.is(EnumDeclSyntax.self) || scopeNode.is(ActorDeclSyntax.self) {
                                        break
                                    }

                                    // Move to the parent node for the next iteration
                                    currentScopeNode = scopeNode.parent
                                }
                            }
                            // END REPLACEMENT
                            // Final decision: Need prefix if potentially instance member AND NOT declared locally (param or var/let OR catch error)
                            if isPotentiallyInstanceMemberAccess && !isDeclaredLocally {
                                needsSelfPrefix = true
                            }
                        }

                        if needsSelfPrefix {
                            exprTextForLog = "self.\(rawExprText)"
                        }
                        // --- End check ---

                        argumentText += " \\(\(rawExprText)) " // Use raw text for keyword check
                        logMessageCode += "\\(\(exprTextForLog))" // Use potentially prefixed text for logger code
                    }
                }
            } else {
                 // Handle non-string literal arguments (e.g., print(myVar), print(getVal()))
                 // Remove unused variable
                 // containsInterpolation = true // Assume non-literals might be interpolated later by logger
                 let interpolatedExpr = expressionToProcess
                 let rawExprText = interpolatedExpr.description
                 var exprTextForLog = rawExprText // Text to use in the logger string

                 // --- Check if 'self.' should be prepended ---
                 var needsSelfPrefix = false // Default: No prefix

                 // Condition 1: Are we in a context where 'self.' might be needed? (Class/Actor, non-generic)
                 let selfPrefixPotentiallyNeeded = !context.isGeneric && (context.typeKind == .classDecl || context.typeKind == .actorDecl)

                 if selfPrefixPotentiallyNeeded {
                     // Condition 2: Is the expression potentially referring to an instance member?
                     var isPotentiallyInstanceMemberAccess = false
                     var identifierToCheckForLocalScope: TokenSyntax? = nil // Track identifier if simple

                     if let simpleIdentifier = interpolatedExpr.as(DeclReferenceExprSyntax.self) {
                         // It's 'foo'. Assume it MIGHT be an instance member.
                         isPotentiallyInstanceMemberAccess = true
                         identifierToCheckForLocalScope = simpleIdentifier.baseName
                     } else if let memberAccess = interpolatedExpr.as(MemberAccessExprSyntax.self) {
                         if memberAccess.base == nil {
                             // Implicit member '.foo' -> YES
                             isPotentiallyInstanceMemberAccess = true
                         }
                         // else: Explicit member access like 'a.b' or 'self.b'. (See comment in the other block)
                     } // else: Literals, function calls, etc. -> No

                     // Condition 3: Check if the identifier (if found) is a local parameter.
                      // START REPLACEMENT
                      var isDeclaredLocally = false
                      if let ident = identifierToCheckForLocalScope {
                          var currentScopeNode: Syntax? = Syntax(node) // Start from the print call

                          while let scopeNode = currentScopeNode {
                              // 1. Check function/method/initializer/subscript parameters
                              if let funcDecl = scopeNode.as(FunctionDeclSyntax.self) {
                                  if funcDecl.signature.parameterClause.parameters.contains(where: { $0.firstName.text == ident.text || $0.secondName?.text == ident.text }) {
                                      isDeclaredLocally = true; break
                                  }
                              } else if let initDecl = scopeNode.as(InitializerDeclSyntax.self) {
                                  if initDecl.signature.parameterClause.parameters.contains(where: { $0.firstName.text == ident.text || $0.secondName?.text == ident.text }) {
                                      isDeclaredLocally = true; break
                                  }
                              } else if let subscriptDecl = scopeNode.as(SubscriptDeclSyntax.self) {
                                  if subscriptDecl.parameterClause.parameters.contains(where: { $0.firstName.text == ident.text || $0.secondName?.text == ident.text }) {
                                      isDeclaredLocally = true; break
                                  }
                              }
                              // 2. Check closure parameters
                              else if let closureExpr = scopeNode.as(ClosureExprSyntax.self) {
                                  // Check formal parameters
                                  if let params = closureExpr.signature?.parameterClause?.as(ClosureParameterClauseSyntax.self) {
                                      if params.parameters.contains(where: { param in param.firstName.text == ident.text }) {
                                          isDeclaredLocally = true; break // Found formal closure param
                                      }
                                  }
                                  // Check shorthand parameters
                                  else if closureExpr.signature?.parameterClause?.is(ClosureShorthandParameterListSyntax.self) ?? false {
                                       if ident.text.hasPrefix("$") && Int(ident.text.dropFirst()) != nil {
                                           isDeclaredLocally = true; break // Found shorthand closure param
                                       }
                                  }
                              }
                              // 3. Check 'catch' block variable
                              else if let catchClause = scopeNode.as(CatchClauseSyntax.self) {
                                  // Check for 'catch let errorName' or implicit 'error'
                                  let catchItemList = catchClause.catchItems
                                  // Remove unused variable
                                  // var foundInCatch = false
                                  var catchErrorName: String? = nil // nil means implicit 'error'
                                  if catchItemList.isEmpty || (catchItemList.count == 1 && catchItemList.first?.pattern == nil) {
                                      // Simple 'catch' -> implicit 'error'
                                      catchErrorName = nil
                                  } else {
                                       for item in catchItemList {
                                           if let pattern = item.pattern {
                                               if let bindingPattern = pattern.as(ValueBindingPatternSyntax.self),
                                                  let identifierPattern = bindingPattern.pattern.as(IdentifierPatternSyntax.self) {
                                                    catchErrorName = identifierPattern.identifier.text; break
                                               } else if let identifierPattern = pattern.as(IdentifierPatternSyntax.self) {
                                                    catchErrorName = identifierPattern.identifier.text; break
                                               } // Add more complex pattern checks if needed
                                           }
                                       }
                                  }
                                   if (catchErrorName == nil && ident.text == "error") || (catchErrorName != nil && ident.text == catchErrorName) {
                                        isDeclaredLocally = true; break
                                   }
                              }
                              // 4. Check 'for' loop variable
                              else if let forStmt = scopeNode.as(ForStmtSyntax.self) {
                                  // Is the print call *inside* this loop's body?
                                  if node.position >= forStmt.body.position && node.endPosition <= forStmt.body.endPosition {
                                      // Does the identifier match the loop variable pattern?
                                      if let idPattern = forStmt.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                           isDeclaredLocally = true; break
                                      }
                                      if let tuplePattern = forStmt.pattern.as(TuplePatternSyntax.self) {
                                           for element in tuplePattern.elements {
                                               if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                   isDeclaredLocally = true; break
                                               }
                                           }
                                           if isDeclaredLocally { break }
                                      }
                                  }
                              }
                              // 5. Check 'guard let/var' bindings *if the print call is AFTER the guard*
                              else if let guardStmt = scopeNode.as(GuardStmtSyntax.self) {
                                  if node.position > guardStmt.endPosition {
                                      for condition in guardStmt.conditions {
                                           if let optBinding = condition.condition.as(OptionalBindingConditionSyntax.self) {
                                                if let idPattern = optBinding.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                    isDeclaredLocally = true; break
                                                }
                                                if let tuplePattern = optBinding.pattern.as(TuplePatternSyntax.self) {
                                                     for element in tuplePattern.elements {
                                                         if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                             isDeclaredLocally = true; break
                                                         }
                                                     }
                                                     if isDeclaredLocally { break }
                                                }
                                           }
                                       }
                                      if isDeclaredLocally { break }
                                  }
                              }
                              // 6. Check 'if let/var' bindings *if the print call is INSIDE the relevant block*
                              else if let ifStmt = scopeNode.as(IfExprSyntax.self) { // CORRECTED TYPE
                                  var foundInIfCondition = false
                                  for condition in ifStmt.conditions {
                                       if let optBinding = condition.condition.as(OptionalBindingConditionSyntax.self) {
                                            if let idPattern = optBinding.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                if node.position >= ifStmt.body.position && node.endPosition <= ifStmt.body.endPosition {
                                                    isDeclaredLocally = true; foundInIfCondition = true; break
                                                }
                                            }
                                            if let tuplePattern = optBinding.pattern.as(TuplePatternSyntax.self) {
                                                 for element in tuplePattern.elements {
                                                     if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                          if node.position >= ifStmt.body.position && node.endPosition <= ifStmt.body.endPosition {
                                                              isDeclaredLocally = true; foundInIfCondition = true; break
                                                          }
                                                     }
                                                 }
                                                 if foundInIfCondition { break }
                                            }
                                       }
                                   }
                                  if foundInIfCondition { break }
                                   if let elseBody = ifStmt.elseBody { // Handle else/else-if cases via upward traversal
                                       // No direct binding check needed here, outer loop handles enclosing scopes.
                                   }
                              }

                              // 7. Check local 'let'/'var' declarations within the current code block scope
                              var currentCodeBlock: CodeBlockSyntax? = nil
                              if let funcDecl = scopeNode.as(FunctionDeclSyntax.self) { currentCodeBlock = funcDecl.body }
                              else if let initDecl = scopeNode.as(InitializerDeclSyntax.self) { currentCodeBlock = initDecl.body }
                              else if let closureExpr = scopeNode.as(ClosureExprSyntax.self) { currentCodeBlock = CodeBlockSyntax(statements: closureExpr.statements) }
                              else if let ifStmt = scopeNode.as(IfExprSyntax.self) { // CORRECTED TYPE
                                  // IfExprSyntax uses body: CodeBlockSyntax
                                  if node.position >= ifStmt.body.position && node.endPosition <= ifStmt.body.endPosition { currentCodeBlock = ifStmt.body }
                                  // IfExprSyntax uses elseBody: Syntax? which can be CodeBlockSyntax or IfExprSyntax
                                  else if let elseSyntax = ifStmt.elseBody {
                                       if let elseBody = elseSyntax.as(CodeBlockSyntax.self), node.position >= elseBody.position && node.endPosition <= elseBody.endPosition { currentCodeBlock = elseBody }
                                       // else-if case is handled by the next iteration of the while loop
                                  }
                              }
                              else if let forStmt = scopeNode.as(ForStmtSyntax.self) { currentCodeBlock = forStmt.body }
                              else if let whileStmt = scopeNode.as(WhileStmtSyntax.self) { currentCodeBlock = whileStmt.body }
                              else if let repeatStmt = scopeNode.as(RepeatStmtSyntax.self) { currentCodeBlock = repeatStmt.body }
                              else if let doStmt = scopeNode.as(DoStmtSyntax.self) { currentCodeBlock = doStmt.body }
                              else if let accessorDecl = scopeNode.as(AccessorDeclSyntax.self) { currentCodeBlock = accessorDecl.body }
                              else if let catchClause = scopeNode.as(CatchClauseSyntax.self) { currentCodeBlock = catchClause.body }

                              if let codeBlock = currentCodeBlock {
                                  for stmtItem in codeBlock.statements {
                                       if stmtItem.position >= node.position { break } // Stop searching this block past the print call
                                       if let varDecl = stmtItem.item.as(VariableDeclSyntax.self) {
                                           for binding in varDecl.bindings {
                                               if let idPattern = binding.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                   isDeclaredLocally = true; break
                                               }
                                               if let tuplePattern = binding.pattern.as(TuplePatternSyntax.self) {
                                                    for element in tuplePattern.elements {
                                                        if let idPattern = element.pattern.as(IdentifierPatternSyntax.self), idPattern.identifier.text == ident.text {
                                                            isDeclaredLocally = true; break
                                                        }
                                                    }
                                               }
                                                if isDeclaredLocally { break }
                                           }
                                       }
                                       if isDeclaredLocally { break } // Break inner statement loop
                                  }
                                   if isDeclaredLocally { break } // Break outer while loop
                              }
                              
                              if scopeNode.is(ClassDeclSyntax.self) || scopeNode.is(StructDeclSyntax.self) || scopeNode.is(EnumDeclSyntax.self) || scopeNode.is(ActorDeclSyntax.self) {
                                  break // Stop at type boundary
                              }
                              currentScopeNode = scopeNode.parent
                          }
                      }
                     // END REPLACEMENT
                     // Final decision: Need prefix if potentially instance member AND NOT declared locally
                     if isPotentiallyInstanceMemberAccess && !isDeclaredLocally {
                         needsSelfPrefix = true
                     }
                 }

                 if needsSelfPrefix {
                      exprTextForLog = "self.\(rawExprText)"
                 }
                 // --- End check ---

                 argumentText += " \\(\(rawExprText)) " // Use raw text for keyword check
                 logMessageCode += "\\(\(exprTextForLog))" // Use potentially prefixed text for logger code
            }

            // Check keywords in the constructed argument text (lowercase)
            let lowercaseArgText = argumentText.lowercased()
            if detectedLevel == nil || logLevelPriority(detectedLevel!) < logLevelPriority("fault") {
                 if faultKeywords.contains(where: { lowercaseArgText.contains($0) }) {
                    detectedLevel = "fault"
                }
            }
             if detectedLevel == nil || logLevelPriority(detectedLevel!) < logLevelPriority("error") {
                if errorKeywords.contains(where: { lowercaseArgText.contains($0) }) {
                    detectedLevel = "error"
                }
            }
             if detectedLevel == nil || logLevelPriority(detectedLevel!) < logLevelPriority("warning") {
                // In os.Logger, warning maps to error, but we use the distinct method name
                if warningKeywords.contains(where: { lowercaseArgText.contains($0) }) {
                     detectedLevel = "warning"
                }
            }
            if detectedLevel == nil || logLevelPriority(detectedLevel!) < logLevelPriority("debug") {
                if debugKeywords.contains(where: { lowercaseArgText.contains($0) }) {
                    detectedLevel = "debug"
                }
            }
             // We don't explicitly check for info/notice as it's the fallback
        }
        logMessageCode += "\""

        // Override catch block default if a lower level keyword was found
        if let level = detectedLevel {
            // Only override if the detected level is lower priority than the default for the context
            if !inCatchBlock || logLevelPriority(level) < logLevelPriority(logLevel) {
                 logLevel = level
            }
        }
        
        // Determine logger call name and prefix
        var loggerNameToUse: String
        var loggerCallPrefix = "" // e.g., self. or TypeName.

        if context.isGeneric {
             loggerNameToUse = (context.typeName ?? "unknownType") + "Logger" // e.g., MyGenericStructLogger
             loggerCallPrefix = "" // File-level access, no prefix
        } else {
             loggerNameToUse = "logger" // Standard name for non-generic
             switch context.typeKind {
             case .structDecl, .enumDecl:
                 // Static logger access: TypeName.logger
                 if let name = context.typeName { // Use the directly found type name
                     loggerCallPrefix = name + "."
                 } else {
                     loggerCallPrefix = "" // Should have type name for static access
                 }
             case .classDecl, .actorDecl:
                 // Instance logger access: self.logger or logger
                  if context.isInClosure || isInsideTaskClosure {
                      loggerCallPrefix = "self."
                  } else {
                      loggerCallPrefix = "" // Implicit self
                  }
             default:
                 // Global context or Task block potentially outside known type
                  if isInsideTaskClosure { // Assumed to be in a class/actor context if in Task
                      loggerCallPrefix = "self."
                       loggerNameToUse = "logger" // Assume standard name exists via self
                  } else {
                      // Cannot determine prefix or logger name reliably
                      loggerCallPrefix = ""
                      loggerNameToUse = "logger" // Best guess
                 }
             }
        }

        // Use the determined log level directly as the method name.
        let loggerMethod = logLevel
        // Ensure prefix ends with "." if it's not empty and requires it
        if !loggerCallPrefix.isEmpty && !loggerCallPrefix.hasSuffix(".") {
             loggerCallPrefix += "." // Should only happen for TypeName.
        }
        let loggerCallCode = "\(loggerCallPrefix)\(loggerNameToUse).\(loggerMethod)(\(logMessageCode))"

        var newCallExpr: ExprSyntax = ExprSyntax("\(raw: loggerCallCode)")
        if let firstToken = node.firstToken(viewMode: .sourceAccurate),
           let lastToken = node.lastToken(viewMode: .sourceAccurate) {
            newCallExpr = newCallExpr
                .with(\.leadingTrivia, firstToken.leadingTrivia)
                .with(\.trailingTrivia, lastToken.trailingTrivia)
        }
        return super.visit(newCallExpr)
    }
    
    // Helper function to check if a CodeBlockItem is a relevant local variable declaration
    // REMOVE THIS FUNCTION - NO LONGER USED
    // private func isLocalVariableDeclaration(_ item: CodeBlockItemSyntax, identifier: String, before node: FunctionCallExprSyntax) -> Bool { ... }

    // Helper function to assign priority to log levels
    private func logLevelPriority(_ level: String) -> Int {
        switch level {
        case "fault": return 5
        case "error": return 4
        case "warning": return 3 // Conceptually distinct for keyword detection
        case "info": return 2
        case "debug": return 1
        default: return 0
        }
    }

    override func visit(_ node: ClassDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        // Note: Classes can be generic, but instance members access `self.logger` anyway.
        // We only need special handling for static loggers in generic structs/enums.
        // Reset generic flag for classes/actors.
        self.isCurrentTypeGeneric = false
        let typeName = node.name.text // Store before super.visit might clear

        guard let transformedClass = super.visit(node).as(ClassDeclSyntax.self) else {
            // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(node)
        }

        var loggerVarDecl: VariableDeclSyntax? = nil
        var loggerMemberIndex: Int? = nil
        var firstNonVarDeclIndex: Int? = nil // Find the first func/init/subscript/nested type
        
        // Iterate through members to find existing logger AND first non-var index
        for (index, member) in transformedClass.memberBlock.members.enumerated() {
            let decl = member.decl

            // Check for existing logger
            if loggerVarDecl == nil, let varDecl = decl.as(VariableDeclSyntax.self) {
                for binding in varDecl.bindings {
                    if let pattern = binding.pattern.as(IdentifierPatternSyntax.self),
                       pattern.identifier.text == "logger" {
                        loggerVarDecl = varDecl
                        loggerMemberIndex = index
                        // Don't break, continue to find firstNonVarDeclIndex
                    }
                }
            }

            // Check for first non-variable member
            if firstNonVarDeclIndex == nil {
                 if decl.is(FunctionDeclSyntax.self) || 
                   decl.is(InitializerDeclSyntax.self) || 
                   decl.is(SubscriptDeclSyntax.self) || 
                   decl.is(ClassDeclSyntax.self) || 
                   decl.is(StructDeclSyntax.self) || 
                   decl.is(EnumDeclSyntax.self) ||
                   decl.is(TypeAliasDeclSyntax.self) || // Added Actor
                   decl.is(ActorDeclSyntax.self) {      // Added Actor
                    firstNonVarDeclIndex = index
                }
            }
        }

        // --- Modify Existing Logger OR Insert New One ---
        var updatedMembers = transformedClass.memberBlock.members
        // let typeName = transformedClass.name.text // Moved up

        // Check if this type actually contained print statements
        let hasPrintStatements = typesContainingPrint.contains(typeName)

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify it if needed (ONLY subsystem) - ALWAYS do this
            // DO NOT change protection level or add/remove static here
            guard let binding = existingLoggerDecl.bindings.first, // Assume single binding logger = ...
                  let initializer = binding.initializer,
                  let callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil
                self.currentTypeKind = nil
                self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(transformedClass) // Return unchanged
            }

            var subsystemArgExists = false
            var needsModification = false
            var updatedArgs = callExpr.arguments

            for (argIndex, arg) in callExpr.arguments.enumerated() {
                if arg.label?.text == "subsystem" {
                    subsystemArgExists = true
                    // Check if the value needs updating
                    if arg.expression.description != "\"\(moduleName)\"" {
                         needsModification = true
                         let newExpr = StringLiteralExprSyntax(content: moduleName)
                         let updatedArg = arg.with(\.expression, ExprSyntax(newExpr))
                         let listIndex = updatedArgs.index(updatedArgs.startIndex, offsetBy: argIndex)
                         updatedArgs[listIndex] = updatedArg
                    }
                    break // Found subsystem
                }
            }

            if !subsystemArgExists {
                // Add the subsystem argument at the beginning
                var subsystemExpr = LabeledExprSyntax(label: "subsystem",
                                                      colon: .colonToken(trailingTrivia: .space),
                                                      expression: ExprSyntax(StringLiteralExprSyntax(content: moduleName)))
                if !updatedArgs.isEmpty { subsystemExpr = subsystemExpr.with(\.trailingComma, .commaToken(trailingTrivia: .space)) }
                updatedArgs.insert(subsystemExpr, at: updatedArgs.startIndex)
                needsModification = true
            }

            if needsModification {
                 // Reconstruct ONLY if the subsystem was wrong or missing
                let newCallExpr = callExpr.with(\.arguments, updatedArgs)
                let newInitializer = initializer.with(\.value, ExprSyntax(newCallExpr))
                let newBinding = binding.with(\.initializer, newInitializer)
                let newLoggerDecl = existingLoggerDecl.with(\.bindings, [newBinding]) // Assuming single binding

                let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))
                
                 let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                 let updatedClass = transformedClass.with(\.memberBlock, newMemberBlock)
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(updatedClass)
            } else {
                 // Logger exists and is correct, return unchanged
                self.currentTypeName = nil
                self.currentTypeKind = nil
                self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(transformedClass)
            }

        } else if hasPrintStatements { // ONLY insert if the type had print statements
            // Logger does not exist, insert a new one: internal let logger = ...
            let loggerDeclCode = "internal let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for class \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
                    self.isCurrentTypeGeneric = false // Reset on exit
                    return DeclSyntax(transformedClass) // Return unchanged on error
                }
                loggerMember = MemberBlockItemSyntax(decl: varDecl)
            }

            // Determine insertion index and trivia (existing logic)
            var indentTrivia: Trivia = .spaces(4) // Default indent
            let insertionIndex = firstNonVarDeclIndex ?? updatedMembers.count

            // Determine indent based on the member *before* the insertion point...
            if insertionIndex > 0, let previousMember = updatedMembers.dropLast(updatedMembers.count - insertionIndex).last {
                 indentTrivia = previousMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0)
            } else if let firstMember = updatedMembers.first {
                 indentTrivia = firstMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0)
            }
            
            // Check if preceding members have blank lines...
            var precedingMembersAreSpacedOut = false
            if insertionIndex > 1 { 
                for i in 1..<insertionIndex {
                    let idx = updatedMembers.index(updatedMembers.startIndex, offsetBy: i)
                    if updatedMembers[idx].leadingTrivia.containsNewlines(count: 2) {
                        precedingMembersAreSpacedOut = true
                        break
                    }
                }
            }

            // Prepare the logger declaration with appropriate trivia
            var loggerDeclWithTrivia = loggerMember
            var newlinesBeforeLogger = 1
            if precedingMembersAreSpacedOut {
                newlinesBeforeLogger = 2
            } else if insertionIndex > 0 {
                let previousMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex - 1)
                if !updatedMembers[previousMemberIndex].decl.is(VariableDeclSyntax.self) {
                     newlinesBeforeLogger = 2
                }
            }
            
            loggerDeclWithTrivia.leadingTrivia = .newlines(newlinesBeforeLogger) + indentTrivia
            loggerDeclWithTrivia.trailingTrivia = Trivia()

            // Insert the logger
            updatedMembers.insert(loggerDeclWithTrivia, at: updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex))

            // Adjust the NEXT member's leading trivia...
            let nextMemberRealIndex = insertionIndex + 1
            if nextMemberRealIndex < updatedMembers.count {
                let nextMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: nextMemberRealIndex)
                let nextMember = updatedMembers[nextMemberIndex]
                let requiredNewlines = nextMember.decl.is(VariableDeclSyntax.self) ? 1 : 2
                let newLeadingTrivia = .newlines(requiredNewlines) + indentTrivia
                var mutableNextMember = nextMember
                mutableNextMember.leadingTrivia = newLeadingTrivia
                updatedMembers[nextMemberIndex] = mutableNextMember
            }

            let newMemberBlock = transformedClass.memberBlock.with(\.members, updatedMembers)
            let updatedClass = transformedClass.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(updatedClass)
        } else {
            // No existing logger and no print statements in this type, return unchanged
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(transformedClass)
        }
    }

    // --- Add visit for ActorDeclSyntax ---
    override func visit(_ node: ActorDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        // Actors can be generic, but like classes, use instance logger.
        self.isCurrentTypeGeneric = false
        let typeName = node.name.text // Store before super.visit

        guard let transformedActor = super.visit(node).as(ActorDeclSyntax.self) else {
            // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(node)
        }

        var loggerVarDecl: VariableDeclSyntax? = nil
        var loggerMemberIndex: Int? = nil
        var firstNonVarDeclIndex: Int? = nil

        for (index, member) in transformedActor.memberBlock.members.enumerated() {
            let decl = member.decl
            if loggerVarDecl == nil, let varDecl = decl.as(VariableDeclSyntax.self) {
                for binding in varDecl.bindings {
                    if let pattern = binding.pattern.as(IdentifierPatternSyntax.self), pattern.identifier.text == "logger" {
                        loggerVarDecl = varDecl
                        loggerMemberIndex = index
                    }
                }
            }
            if firstNonVarDeclIndex == nil {
                if decl.is(FunctionDeclSyntax.self) || decl.is(InitializerDeclSyntax.self) ||
                   decl.is(SubscriptDeclSyntax.self) || decl.is(ClassDeclSyntax.self) ||
                   decl.is(StructDeclSyntax.self) || decl.is(EnumDeclSyntax.self) ||
                   decl.is(TypeAliasDeclSyntax.self) || decl.is(ActorDeclSyntax.self) {
                    firstNonVarDeclIndex = index
                }
            }
        }

        var updatedMembers = transformedActor.memberBlock.members
        // let typeName = transformedActor.name.text // Moved up

        // Check if this type actually contained print statements
        let hasPrintStatements = typesContainingPrint.contains(typeName)

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify ONLY subsystem if needed - ALWAYS do this
             guard let binding = existingLoggerDecl.bindings.first,
                   let initializer = binding.initializer,
                   let callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset on exit
                 return DeclSyntax(transformedActor)
             }
            // (Logic for checking/updating subsystem is identical to ClassDecl)
            var subsystemArgExists = false
            var needsModification = false
            var updatedArgs = callExpr.arguments

            for (argIndex, arg) in callExpr.arguments.enumerated() {
                if arg.label?.text == "subsystem" {
                    subsystemArgExists = true
                    if arg.expression.description != "\"\(moduleName)\"" {
                         needsModification = true
                         let newExpr = StringLiteralExprSyntax(content: moduleName)
                         let updatedArg = arg.with(\.expression, ExprSyntax(newExpr))
                         let listIndex = updatedArgs.index(updatedArgs.startIndex, offsetBy: argIndex)
                         updatedArgs[listIndex] = updatedArg
                    }
                    break
                }
            }

            if !subsystemArgExists {
                var subsystemExpr = LabeledExprSyntax(label: "subsystem", colon: .colonToken(trailingTrivia: .space), expression: ExprSyntax(StringLiteralExprSyntax(content: moduleName)))
                if !updatedArgs.isEmpty { subsystemExpr = subsystemExpr.with(\.trailingComma, .commaToken(trailingTrivia: .space)) }
                updatedArgs.insert(subsystemExpr, at: updatedArgs.startIndex)
                needsModification = true
            }

            if needsModification {
                 let newCallExpr = callExpr.with(\.arguments, updatedArgs)
                 let newInitializer = initializer.with(\.value, ExprSyntax(newCallExpr))
                 let newBinding = binding.with(\.initializer, newInitializer)
                 let newLoggerDecl = existingLoggerDecl.with(\.bindings, [newBinding])

                 let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                 updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))

                 let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                 let updatedActor = transformedActor.with(\.memberBlock, newMemberBlock)
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset on exit
                 return DeclSyntax(updatedActor)
             } else {
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset on exit
                 return DeclSyntax(transformedActor)
             }
        } else if hasPrintStatements { // ONLY insert if the type had print statements
            // Insert new logger: internal let logger = ...
            let loggerDeclCode = "internal let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for actor \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
                    self.isCurrentTypeGeneric = false // Reset on exit
                    return DeclSyntax(transformedActor)
                }
                loggerMember = MemberBlockItemSyntax(decl: varDecl)
            }
            // (Logic for determining insertion index and trivia is identical to ClassDecl)
             var indentTrivia: Trivia = .spaces(4)
             let insertionIndex = firstNonVarDeclIndex ?? updatedMembers.count
             if insertionIndex > 0, let previousMember = updatedMembers.dropLast(updatedMembers.count - insertionIndex).last { indentTrivia = previousMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0) }
             else if let firstMember = updatedMembers.first { indentTrivia = firstMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0) }
             var precedingMembersAreSpacedOut = false
             if insertionIndex > 1 { for i in 1..<insertionIndex { let idx = updatedMembers.index(updatedMembers.startIndex, offsetBy: i); if updatedMembers[idx].leadingTrivia.containsNewlines(count: 2) { precedingMembersAreSpacedOut = true; break } } }
             var loggerDeclWithTrivia = loggerMember
             var newlinesBeforeLogger = 1
             if precedingMembersAreSpacedOut { newlinesBeforeLogger = 2 }
             else if insertionIndex > 0 { let previousMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex - 1); if !updatedMembers[previousMemberIndex].decl.is(VariableDeclSyntax.self) { newlinesBeforeLogger = 2 } }
             loggerDeclWithTrivia.leadingTrivia = .newlines(newlinesBeforeLogger) + indentTrivia
             loggerDeclWithTrivia.trailingTrivia = Trivia()
             updatedMembers.insert(loggerDeclWithTrivia, at: updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex))
             let nextMemberRealIndex = insertionIndex + 1
             if nextMemberRealIndex < updatedMembers.count {
                 let nextMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: nextMemberRealIndex)
                 let nextMember = updatedMembers[nextMemberIndex]
                 let nextDecl = nextMember.decl
                 // Adjust for enum cases when determining spacing
                 // Need 1 newline if the next member IS a var or enum case, 2 otherwise
                 let requiredNewlines = (nextDecl.is(VariableDeclSyntax.self) || nextDecl.is(EnumCaseDeclSyntax.self)) ? 1 : 2
                 // Adjust trivia of next element ONLY IF it exists
                 if let nextElementIndent = nextMember.leadingTrivia.indentation(isOnNewline: true) {
                    let newLeadingTrivia = .newlines(requiredNewlines) + nextElementIndent
                    var mutableNextMember = nextMember
                    mutableNextMember.leadingTrivia = newLeadingTrivia
                    updatedMembers[nextMemberIndex] = mutableNextMember
                }
             }

            let newMemberBlock = transformedActor.memberBlock.with(\.members, updatedMembers)
            let updatedActor = transformedActor.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(updatedActor)
        } else {
            // No existing logger and no print statements in this type, return unchanged
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(transformedActor)
        }
    }

    override func visit(_ node: StructDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        // Check if THIS struct is generic
        let isGeneric = node.genericParameterClause != nil
        self.isCurrentTypeGeneric = isGeneric // Set context for children
        let typeName = node.name.text // Store before super.visit clears context

        // Perform super.visit FIRST to handle nested types correctly
        guard let transformedStruct = super.visit(node).as(StructDeclSyntax.self) else {
             // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(node)
        }

        var loggerVarDecl: VariableDeclSyntax? = nil
        var loggerMemberIndex: Int? = nil
        var firstNonVarDeclIndex: Int? = nil // Find the first func/init/subscript/nested type
        
        // Iterate through members to find existing logger AND first non-var index
        // Create a mutable copy to potentially remove the logger if generic
        var currentMembers = Array(transformedStruct.memberBlock.members)
        var memberIndexOffset = 0 // Adjust index if we remove items
        for (index, member) in transformedStruct.memberBlock.members.enumerated() {
            let decl = member.decl

            // Check for existing logger
            if loggerVarDecl == nil, let varDecl = decl.as(VariableDeclSyntax.self) {
                for binding in varDecl.bindings {
                    if let pattern = binding.pattern.as(IdentifierPatternSyntax.self),
                       pattern.identifier.text == "logger" {
                        loggerVarDecl = varDecl
                        loggerMemberIndex = index
                        // Don't break, continue to find firstNonVarDeclIndex
                    }
                }
            }

            // Check for first non-variable member
            if firstNonVarDeclIndex == nil {
                 if decl.is(FunctionDeclSyntax.self) || 
                   decl.is(InitializerDeclSyntax.self) || 
                   decl.is(SubscriptDeclSyntax.self) || 
                   decl.is(ClassDeclSyntax.self) || 
                   decl.is(StructDeclSyntax.self) || 
                   decl.is(EnumDeclSyntax.self) ||
                   decl.is(TypeAliasDeclSyntax.self) || // Added Actor
                   decl.is(ActorDeclSyntax.self) {      // Added Actor
                    firstNonVarDeclIndex = index
                }
            }
        }

        // --- Modify Existing Logger OR Insert New One ---
        // let typeName = transformedStruct.name.text // Moved up

        // Check if this type actually contained print statements
        let hasPrintStatements = typesContainingPrint.contains(typeName)

        // If the struct is generic AND had print statements, queue file-level logger and REMOVE internal one
        if isGeneric && hasPrintStatements {
            let loggerDeclCode = "fileprivate let \(typeName)Logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            pendingFileLevelLoggers[typeName] = loggerDeclCode
            // print(">>> Queued file logger for generic struct \\(typeName)") // DEBUG

            // Remove the existing static logger if found
            if let idx = loggerMemberIndex {
                 let actualIndex = currentMembers.index(currentMembers.startIndex, offsetBy: idx - memberIndexOffset)
                 // print(">>> Removing existing static logger from generic struct \\(typeName)") // DEBUG
                currentMembers.remove(at: actualIndex)
                memberIndexOffset += 1
                // Note: Trivia adjustment might be needed if removing affects spacing significantly.
                // For simplicity, we assume basic removal is sufficient for now.
            }
            // Return the struct with potentially removed logger, but no added one
             let newMemberBlock = transformedStruct.memberBlock.with(\.members, MemberBlockItemListSyntax(currentMembers))
             let updatedStruct = transformedStruct.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil // Clear context
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset flag
            return DeclSyntax(updatedStruct)
        }

        // --- Handle: Non-Generic OR Generic without print statements ---
        var updatedMembers = MemberBlockItemListSyntax(currentMembers) // Use potentially modified list if generic logic ran (though it returns early now)

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify ONLY subsystem if needed - ALWAYS do this
            // (Even if generic and no print, or non-generic and no print)
            guard let binding = existingLoggerDecl.bindings.first,
                  let initializer = binding.initializer,
                  let callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil
                self.currentTypeKind = nil
                self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(transformedStruct) // Return unchanged
            }
            // (Logic for checking/updating subsystem is identical to ClassDecl/StructDecl)
            var subsystemArgExists = false
            var needsModification = false
            var updatedArgs = callExpr.arguments

            for (argIndex, arg) in callExpr.arguments.enumerated() {
                if arg.label?.text == "subsystem" {
                    subsystemArgExists = true
                    if arg.expression.description != "\"\(moduleName)\"" {
                         needsModification = true
                         let newExpr = StringLiteralExprSyntax(content: moduleName)
                         let updatedArg = arg.with(\.expression, ExprSyntax(newExpr))
                         let listIndex = updatedArgs.index(updatedArgs.startIndex, offsetBy: argIndex)
                         updatedArgs[listIndex] = updatedArg
                    }
                    break
                }
            }

            if !subsystemArgExists {
                var subsystemExpr = LabeledExprSyntax(label: "subsystem", colon: .colonToken(trailingTrivia: .space), expression: ExprSyntax(StringLiteralExprSyntax(content: moduleName)))
                if !updatedArgs.isEmpty { subsystemExpr = subsystemExpr.with(\.trailingComma, .commaToken(trailingTrivia: .space)) }
                updatedArgs.insert(subsystemExpr, at: updatedArgs.startIndex)
                needsModification = true
            }

            if needsModification {
                 let newCallExpr = callExpr.with(\.arguments, updatedArgs)
                 let newInitializer = initializer.with(\.value, ExprSyntax(newCallExpr))
                 let newBinding = binding.with(\.initializer, newInitializer)
                 // Preserve existing modifiers (static, protection level)
                 let newLoggerDecl = existingLoggerDecl.with(\.bindings, [newBinding])

                 let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                 updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))

                 let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                 let updatedStruct = transformedStruct.with(\.memberBlock, newMemberBlock)
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset on exit
                 return DeclSyntax(updatedStruct)
             } else {
                 // Logger exists and is correct, return unchanged
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset on exit
                 return DeclSyntax(transformedStruct)
             }

        } else if !isGeneric && hasPrintStatements { // ONLY insert if non-generic AND had print
            // Logger does not exist, insert a new one: internal static let logger = ...
            let loggerDeclCode = "internal static let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for struct \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
                    self.isCurrentTypeGeneric = false // Reset on exit
                    return DeclSyntax(transformedStruct) // Return unchanged on error
                }
                loggerMember = MemberBlockItemSyntax(decl: varDecl)
            }

             // Determine insertion index and trivia (existing logic remains the same)
            var indentTrivia: Trivia = .spaces(4) // Default indent
            let insertionIndex = firstNonVarDeclIndex ?? updatedMembers.count
            if insertionIndex > 0, let previousMember = updatedMembers.dropLast(updatedMembers.count - insertionIndex).last { indentTrivia = previousMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0) }
            else if let firstMember = updatedMembers.first { indentTrivia = firstMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0) }
            var precedingMembersAreSpacedOut = false
            if insertionIndex > 1 { for i in 1..<insertionIndex { let idx = updatedMembers.index(updatedMembers.startIndex, offsetBy: i); if updatedMembers[idx].leadingTrivia.containsNewlines(count: 2) { precedingMembersAreSpacedOut = true; break } } }
            var loggerDeclWithTrivia = loggerMember
            var newlinesBeforeLogger = 1
            if precedingMembersAreSpacedOut { newlinesBeforeLogger = 2 }
            else if insertionIndex > 0 { let previousMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex - 1); if !updatedMembers[previousMemberIndex].decl.is(VariableDeclSyntax.self) && !updatedMembers[previousMemberIndex].decl.is(EnumCaseDeclSyntax.self) { newlinesBeforeLogger = 2 } } // Adjust for enum cases
            loggerDeclWithTrivia.leadingTrivia = .newlines(newlinesBeforeLogger) + indentTrivia
            loggerDeclWithTrivia.trailingTrivia = Trivia()
            updatedMembers.insert(loggerDeclWithTrivia, at: updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex))
            let nextMemberRealIndex = insertionIndex + 1
            if nextMemberRealIndex < updatedMembers.count {
                let nextMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: nextMemberRealIndex)
                var nextMember = updatedMembers[nextMemberIndex]
                // Adjust for enum cases when determining spacing
                // Need 1 newline if the next member IS a var or enum case, 2 otherwise
                let requiredNewlines = (nextMember.decl.is(VariableDeclSyntax.self) || nextMember.decl.is(EnumCaseDeclSyntax.self)) ? 1 : 2
                // Adjust trivia of next element ONLY IF it exists
                if let nextElementIndent = nextMember.leadingTrivia.indentation(isOnNewline: true) {
                    let newLeadingTrivia = .newlines(requiredNewlines) + nextElementIndent
                    var mutableNextMember = nextMember
                    mutableNextMember.leadingTrivia = newLeadingTrivia
                    updatedMembers[nextMemberIndex] = mutableNextMember
                }
            }

            let newMemberBlock = transformedStruct.memberBlock.with(\.members, updatedMembers)
            let updatedStruct = transformedStruct.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(updatedStruct)
        } else {
            // No existing logger AND ( (generic without print) OR (non-generic without print) )
            // Return unchanged in these cases
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(transformedStruct)
        }
    }

    // --- Add visit for EnumDeclSyntax ---
     override func visit(_ node: EnumDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        // Check if THIS enum is generic
        let isGeneric = node.genericParameterClause != nil
        self.isCurrentTypeGeneric = isGeneric // Set context for children
        let typeName = node.name.text // Store before super.visit

        // Perform super.visit FIRST to handle nested types correctly
        guard let transformedEnum = super.visit(node).as(EnumDeclSyntax.self) else {
            // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(node)
        }

        var loggerVarDecl: VariableDeclSyntax? = nil
        var loggerMemberIndex: Int? = nil
        var firstNonVarDeclIndex: Int? = nil

        // Create a mutable copy to potentially remove the logger if generic
        var currentMembers = Array(transformedEnum.memberBlock.members)
        var memberIndexOffset = 0 // Adjust index if we remove items
        for (index, member) in transformedEnum.memberBlock.members.enumerated() {
            let decl = member.decl
            if loggerVarDecl == nil, let varDecl = decl.as(VariableDeclSyntax.self) {
                for binding in varDecl.bindings {
                    if let pattern = binding.pattern.as(IdentifierPatternSyntax.self), pattern.identifier.text == "logger" {
                        loggerVarDecl = varDecl
                        loggerMemberIndex = index
                    }
                }
            }
             if firstNonVarDeclIndex == nil {
                 if decl.is(FunctionDeclSyntax.self) || decl.is(InitializerDeclSyntax.self) ||
                    decl.is(SubscriptDeclSyntax.self) || decl.is(ClassDeclSyntax.self) ||
                    decl.is(StructDeclSyntax.self) || decl.is(EnumDeclSyntax.self) ||
                    decl.is(TypeAliasDeclSyntax.self) || decl.is(ActorDeclSyntax.self) ||
                    decl.is(EnumCaseDeclSyntax.self) { // Check for EnumCaseDeclSyntax here is correct for finding first *non* case/var/etc.
                     firstNonVarDeclIndex = index
                 }
             }
        }

        // let typeName = transformedEnum.name.text // Moved up

        // Check if this type actually contained print statements
        let hasPrintStatements = typesContainingPrint.contains(typeName)

        // If the enum is generic AND had print statements, queue file-level logger and REMOVE internal one
        if isGeneric && hasPrintStatements {
            let loggerDeclCode = "fileprivate let \(typeName)Logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            pendingFileLevelLoggers[typeName] = loggerDeclCode
             // print(">>> Queued file logger for generic enum \\(typeName)") // DEBUG

            // Remove the existing static logger if found
            if let idx = loggerMemberIndex {
                 let actualIndex = currentMembers.index(currentMembers.startIndex, offsetBy: idx - memberIndexOffset)
                 // print(">>> Removing existing static logger from generic enum \\(typeName)") // DEBUG
                currentMembers.remove(at: actualIndex)
                memberIndexOffset += 1
                // Trivia adjustment might be needed here too.
            }
            // Return the enum with potentially removed logger, but no added one
            let newMemberBlock = transformedEnum.memberBlock.with(\.members, MemberBlockItemListSyntax(currentMembers))
            let updatedEnum = transformedEnum.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil // Clear context
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset flag
            return DeclSyntax(updatedEnum)
        }

        // --- Handle: Non-Generic OR Generic without print statements ---
        var updatedMembers = MemberBlockItemListSyntax(currentMembers) // Use potentially modified list

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify ONLY subsystem if needed - ALWAYS do this
            guard let binding = existingLoggerDecl.bindings.first,
                  let initializer = binding.initializer,
                  let callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil
                self.currentTypeKind = nil
                self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(transformedEnum) // Return unchanged
            }
            // (Logic for checking/updating subsystem is identical to ClassDecl/StructDecl)
            var subsystemArgExists = false
            var needsModification = false
            var updatedArgs = callExpr.arguments

            for (argIndex, arg) in callExpr.arguments.enumerated() {
                if arg.label?.text == "subsystem" {
                    subsystemArgExists = true
                    if arg.expression.description != "\"\(moduleName)\"" {
                         needsModification = true
                         let newExpr = StringLiteralExprSyntax(content: moduleName)
                         let updatedArg = arg.with(\.expression, ExprSyntax(newExpr))
                         let listIndex = updatedArgs.index(updatedArgs.startIndex, offsetBy: argIndex)
                         updatedArgs[listIndex] = updatedArg
                    }
                    break
                }
            }

            if !subsystemArgExists {
                var subsystemExpr = LabeledExprSyntax(label: "subsystem", colon: .colonToken(trailingTrivia: .space), expression: ExprSyntax(StringLiteralExprSyntax(content: moduleName)))
                if !updatedArgs.isEmpty { subsystemExpr = subsystemExpr.with(\.trailingComma, .commaToken(trailingTrivia: .space)) }
                updatedArgs.insert(subsystemExpr, at: updatedArgs.startIndex)
                needsModification = true
            }

            if needsModification {
                let newCallExpr = callExpr.with(\.arguments, updatedArgs)
                let newInitializer = initializer.with(\.value, ExprSyntax(newCallExpr))
                let newBinding = binding.with(\.initializer, newInitializer)
                let newLoggerDecl = existingLoggerDecl.with(\.bindings, [newBinding]) // Preserve existing modifiers

                let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))

                let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                let updatedEnum = transformedEnum.with(\.memberBlock, newMemberBlock)
                self.currentTypeName = nil
                self.currentTypeKind = nil
                self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(updatedEnum)
            } else {
                self.currentTypeName = nil
                self.currentTypeKind = nil
                self.isCurrentTypeGeneric = false // Reset on exit
                return DeclSyntax(transformedEnum)
            }
        } else if !isGeneric && hasPrintStatements { // ONLY insert if non-generic AND had print
            // Insert new logger: internal static let logger = ...
            let loggerDeclCode = "internal static let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for enum \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
                    self.isCurrentTypeGeneric = false // Reset on exit
                    return DeclSyntax(transformedEnum)
                }
                loggerMember = MemberBlockItemSyntax(decl: varDecl)
            }
            // (Logic for determining insertion index and trivia is identical to StructDecl)
             var indentTrivia: Trivia = .spaces(4)
             let insertionIndex = firstNonVarDeclIndex ?? updatedMembers.count
             if insertionIndex > 0, let previousMember = updatedMembers.dropLast(updatedMembers.count - insertionIndex).last { indentTrivia = previousMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0) }
             else if let firstMember = updatedMembers.first { indentTrivia = firstMember.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0) }
             var precedingMembersAreSpacedOut = false
             if insertionIndex > 1 { for i in 1..<insertionIndex { let idx = updatedMembers.index(updatedMembers.startIndex, offsetBy: i); if updatedMembers[idx].leadingTrivia.containsNewlines(count: 2) { precedingMembersAreSpacedOut = true; break } } }
             var loggerDeclWithTrivia = loggerMember
             var newlinesBeforeLogger = 1
             if precedingMembersAreSpacedOut { newlinesBeforeLogger = 2 }
             else if insertionIndex > 0 {
                 let previousMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex - 1)
                 let previousDecl = updatedMembers[previousMemberIndex].decl
                 // Add 2 newlines if the previous wasn't a var or an enum case
                 if !previousDecl.is(VariableDeclSyntax.self) && !previousDecl.is(EnumCaseDeclSyntax.self) {
                     newlinesBeforeLogger = 2
                 }
             }
             loggerDeclWithTrivia.leadingTrivia = .newlines(newlinesBeforeLogger) + indentTrivia
             loggerDeclWithTrivia.trailingTrivia = Trivia()
             updatedMembers.insert(loggerDeclWithTrivia, at: updatedMembers.index(updatedMembers.startIndex, offsetBy: insertionIndex))
             let nextMemberRealIndex = insertionIndex + 1
             if nextMemberRealIndex < updatedMembers.count {
                 let nextMemberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: nextMemberRealIndex)
                 var nextMember = updatedMembers[nextMemberIndex]
                 let nextDecl = nextMember.decl
                 // Adjust for enum cases when determining spacing
                 // Need 1 newline if the next member IS a var or enum case, 2 otherwise
                 let requiredNewlines = (nextDecl.is(VariableDeclSyntax.self) || nextDecl.is(EnumCaseDeclSyntax.self)) ? 1 : 2
                 // Adjust trivia of next element ONLY IF it exists
                 if let nextElementIndent = nextMember.leadingTrivia.indentation(isOnNewline: true) {
                    let newLeadingTrivia = .newlines(requiredNewlines) + nextElementIndent
                    var mutableNextMember = nextMember
                    mutableNextMember.leadingTrivia = newLeadingTrivia
                    updatedMembers[nextMemberIndex] = mutableNextMember
                }
             }

            let newMemberBlock = transformedEnum.memberBlock.with(\.members, updatedMembers)
            let updatedEnum = transformedEnum.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(updatedEnum)
        } else {
            // No existing logger AND ( (generic without print) OR (non-generic without print) )
            // Return unchanged in these cases
            self.currentTypeName = nil
            self.currentTypeKind = nil
            self.isCurrentTypeGeneric = false // Reset on exit
            return DeclSyntax(transformedEnum)
        }
    }

    // Clear context when leaving a type definition
    override func visitPost(_ node: Syntax) {
        if node.is(ClassDeclSyntax.self) || node.is(StructDeclSyntax.self) || node.is(EnumDeclSyntax.self) || node.is(ActorDeclSyntax.self) {
             // Only clear if the current node matches the one we stored context for
             if self.currentTypeName == (node.asProtocol(NamedDeclSyntax.self)?.name.text),
                self.currentTypeKind == node.kind {
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 self.isCurrentTypeGeneric = false // Reset generic flag too
//                 print("Cleared context for \\(node.kind) \\(node.asProtocol(NamedDeclSyntax.self)?.name.text ?? "")") // DEBUG
             }
        }
    }
}

// MARK: - Main Execution

guard let options = parseOptions() else {
    exit(1)
}

let rootURL = URL(fileURLWithPath: options.rootPath)
// Print the resolved root path
print("Starting scan in directory: \(rootURL.path)")

guard let enumerator = FileManager.default.enumerator(at: rootURL, includingPropertiesForKeys: nil) else {
    fputs("Error: unable to access directory \(options.rootPath)\n", stderr)
    exit(1)
}

let rewriter = PrintToLoggerRewriter(moduleName: options.moduleName)
var modifiedCount = 0

while let fileURL = enumerator.nextObject() as? URL {
    guard fileURL.pathExtension == "swift" else { continue }
    // Print found Swift file
    // print("Found Swift file: \\(fileURL.path)") // Reduce noise
    let sourceFile: SourceFileSyntax
    do {
        // Read the file content
        let sourceCode = try String(contentsOf: fileURL, encoding: .utf8)
        sourceFile = try Parser.parse(source: sourceCode) // Remove unnecessary try
        // Print successful parse
//        print("  Successfully parsed.")
    } catch {
        fputs("  Skipping file \(fileURL.path) due to parsing error: \(error)\n", stderr)
        continue
    }

    // Print that the file is being processed only if changes might occur
    // Simple check: does it contain 'print('? A more robust check could parse first.
    // This avoids printing "Processing" for files with no print statements.
    var containsPrint = false
    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        if content.contains("print(") {
            containsPrint = true
        }
    } catch {
        // ignore read error here, parsing below will catch it
    }
    if !containsPrint && !sourceFile.description.contains(" Logger(") { // Also check if logger already exists to potentially update subsystem
        // print("  Skipping file (no 'print' or 'Logger'): \(fileURL.path)")
        continue // Skip if no print and no existing logger likely needs update
    }
     // Only print if likely to be modified
    
    // Clear the print tracking set for each new file
    rewriter.typesContainingPrint.removeAll() // Reset before visiting

    let transformedSyntax = rewriter.visit(sourceFile)
    // Remove deprecated 'as' cast warning by removing the cast
    guard var transformedFile = transformedSyntax as? SourceFileSyntax else {
         fputs("Rewriter failed to return SourceFileSyntax for \\(fileURL.path)\\n", stderr)
         continue
    }
    
    // --- Insert pending file-level loggers ---
   var statements = transformedFile.statements
   var insertedLoggerCount = 0
   if !rewriter.pendingFileLevelLoggers.isEmpty {
       // Find insertion point (after last import or at beginning)
       var lastImportIndex: Int? = nil
       for (index, stmt) in statements.enumerated().reversed() {
           if stmt.item.is(ImportDeclSyntax.self) {
               lastImportIndex = index
               break
           }
       }
       let insertionFileIndex = lastImportIndex != nil ? statements.index(after: statements.index(statements.startIndex, offsetBy: lastImportIndex!)) : statements.startIndex

       // Determine indent from the element *after* the insertion point, or default
       var indentTrivia: Trivia = .spaces(0) // Default if inserting at end or empty file
       if insertionFileIndex < statements.endIndex {
           let nextElement = statements[insertionFileIndex]
           indentTrivia = nextElement.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0)
       } else if let lastElement = statements.last {
           indentTrivia = lastElement.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0)
       } else if let impIdx = lastImportIndex {
           indentTrivia = statements[statements.index(statements.startIndex, offsetBy: impIdx)].leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0)
       }

       // Insert each logger, sorted by name for deterministic order
       for (typeName, loggerDeclCode) in rewriter.pendingFileLevelLoggers.sorted(by: { $0.key < $1.key }) {
           do {
               guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                     let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                   fputs("Error parsing file-level logger for \\(typeName): \\(loggerDeclCode)\\n", stderr)
                   continue
               }
               var loggerItem = CodeBlockItemSyntax(item: .decl(DeclSyntax(varDecl)))

               // Add trivia: 2 newlines before (unless first element after imports/at start), indent, 1 newline after
               let isFirstElement = insertionFileIndex == statements.startIndex
               let isFirstAfterImports = lastImportIndex != nil && insertionFileIndex == statements.index(after: statements.index(statements.startIndex, offsetBy: lastImportIndex!))

               // Newlines before: 2 unless it's the very first declaration or the first thing after imports
               let newlinesBefore = (isFirstElement || isFirstAfterImports) && insertedLoggerCount == 0 ? 1 : 2

               loggerItem.leadingTrivia = .newlines(newlinesBefore) + indentTrivia
               loggerItem.trailingTrivia = .newlines(1)

               // Insert at the calculated index + offset for previously inserted loggers
               let currentInsertionFileIndex = statements.index(insertionFileIndex, offsetBy: insertedLoggerCount)
               statements.insert(loggerItem, at: currentInsertionFileIndex)
               insertedLoggerCount += 1

           } catch {
               fputs("Error parsing file-level logger for \\(typeName): \\(loggerDeclCode) - \\(error)\\n", stderr)
           }
       }

       // Adjust trivia of the *next* non-logger element if loggers were inserted
       if insertedLoggerCount > 0 {
           let nextElementActualIndex = statements.index(insertionFileIndex, offsetBy: insertedLoggerCount)
           if nextElementActualIndex < statements.endIndex {
               var nextElement = statements[nextElementActualIndex]
               let originalIndent = nextElement.leadingTrivia.indentation(isOnNewline: true) ?? .spaces(0)
               // Ensure it starts with exactly TWO newlines + its original indent after logger block
               let requiredNewlines = 2
               let newLeadingTrivia = .newlines(requiredNewlines) + originalIndent
               var mutableNextElement = nextElement // Create mutable copy
               mutableNextElement.leadingTrivia = newLeadingTrivia
               statements[nextElementActualIndex] = mutableNextElement // Assign back
           }
       }

       // Update the transformed file
       transformedFile = transformedFile.with(\.statements, statements)
       // Clear the pending loggers for the next file
       rewriter.pendingFileLevelLoggers.removeAll()
   }

   // --- Add os.log import (use potentially modified file) ---
   var finalFile = transformedFile
   var hasImport = false
   for stmt in finalFile.statements {
       if let importDecl = stmt.item.as(ImportDeclSyntax.self),
          importDecl.path.description.trimmingCharacters(in: .whitespaces) == "os.log" { // More robust check
           hasImport = true
           break
       }
   }
   if !hasImport {
       let importDecl: ImportDeclSyntax
       importDecl = try! ImportDeclSyntax("import os.log")
       var importItem = CodeBlockItemSyntax(item: .decl(DeclSyntax(importDecl)))
       
       var statements = finalFile.statements
       var lastImportIndex: Int? = nil

       // Find the index of the last import statement
       for (index, stmt) in statements.enumerated().reversed() {
           if stmt.item.is(ImportDeclSyntax.self) {
               lastImportIndex = index
               break
           }
       }

       if let lastIndex = lastImportIndex {
           // Insert after the last import
           let targetIndex = statements.index(statements.startIndex, offsetBy: lastIndex)
           var lastImportItem = statements[targetIndex]
           
           // Ensure the last import has one newline after it
           if !lastImportItem.trailingTrivia.containsNewlines(count: 1) {
                lastImportItem.trailingTrivia = lastImportItem.trailingTrivia + .newlines(1)
           } else {
               // It already has at least one, ensure it's exactly one?
               // Or just keep existing trivia? Keeping existing for now.
           }
           statements[targetIndex] = lastImportItem
           
           // Prepare the new import with indent (if possible), but no leading newline
           let indentTrivia = (lastImportItem.leadingTrivia.indentation(isOnNewline: true) ?? Trivia()) // indentation can return nil
           importItem.leadingTrivia = indentTrivia
           // Add trailing newline to separate from next code block
           importItem.trailingTrivia = .newlines(1) 

           // Insert the new import item
           statements.insert(importItem, at: statements.index(after: targetIndex))
           
           // Ensure the *next* code block starts after ONE newline from the imports
           let nextCodeBlockIndex = statements.index(after: statements.index(after: targetIndex))
           if nextCodeBlockIndex < statements.endIndex {
               var nextBlock = statements[nextCodeBlockIndex]
               // Adjust if it doesn't start with exactly one newline
               if !(nextBlock.leadingTrivia.containsNewlines(count: 1) && !nextBlock.leadingTrivia.containsNewlines(count: 2)) {
                   let indent = nextBlock.leadingTrivia.indentation(isOnNewline: true) ?? Trivia()
                   // Remove extra newlines first, then add exactly one
                   nextBlock.leadingTrivia = .newlines(1) + indent
                   statements[nextCodeBlockIndex] = nextBlock
               }
           }
           
       } else {
           // No existing imports, insert at the beginning
            importItem = importItem.with(\.trailingTrivia, Trivia())
           // Add potential leading trivia from the original first statement
           if let firstStmt = statements.first {
                importItem.leadingTrivia = firstStmt.leadingTrivia.removingNewlinesBeforeFirstCommentOrCode()
                // Ensure ONE newline after the import before the original first statement
               var mutableFirst = firstStmt
               let indent = firstStmt.leadingTrivia.indentation(isOnNewline: true) ?? Trivia()
                mutableFirst.leadingTrivia = .newlines(1) + indent
               statements[statements.startIndex] = mutableFirst
           } else {
                // If the file was empty, just add ONE newline after import
                importItem.trailingTrivia = .newlines(1)
           }
           statements.insert(importItem, at: statements.startIndex)
       }
       
       finalFile = finalFile.with(\.statements, statements)
   }
   
   if options.dryRun && modifiedCount >= 3 { break }
   do {
       try finalFile.description.write(to: fileURL, atomically: true, encoding: .utf8)
       print("Processed: \(fileURL.path)")
       modifiedCount += 1 // Increment only on successful write
   } catch {
       fputs("Failed to write \(fileURL.path): \(error)\n", stderr)
   }
}

print("Processing complete. \(modifiedCount) file(s) modified.")
