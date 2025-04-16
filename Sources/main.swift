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

    init(moduleName: String) {
        self.moduleName = moduleName
    }

    // Helper to find enclosing type and if inside a closure
    private func findEnclosingContext(startNode: Syntax) -> (typeName: String?, typeKind: SyntaxKind?, isInClosure: Bool) {
        var current: Syntax? = startNode.parent
        var enclosingTypeName: String? = nil
        var enclosingTypeKind: SyntaxKind? = nil
        var isInClosure = false

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
                    // Don't break, continue checking for closures further up if needed
                } else if let structDecl = node.as(StructDeclSyntax.self) {
                    enclosingTypeName = structDecl.name.text
                    enclosingTypeKind = .structDecl
                    // Don't break
                } else if let enumDecl = node.as(EnumDeclSyntax.self) {
                    enclosingTypeName = enumDecl.name.text
                    enclosingTypeKind = .enumDecl
                    // Don't break
                } else if let actorDecl = node.as(ActorDeclSyntax.self) {
                    enclosingTypeName = actorDecl.name.text
                    enclosingTypeKind = .actorDecl
                    // Don't break
                }
            }

            // If we've found the type AND determined if we are in a closure, we can potentially stop early,
            // but iterating all the way up is safer to catch all closure contexts.
            // if enclosingTypeName != nil && isInClosure { break }

            current = node.parent
        }
        // Return ONLY the directly found context from walking the tree.
        // Remove fallback to self.currentTypeName/currentTypeKind.
        return (typeName: enclosingTypeName, typeKind: enclosingTypeKind, isInClosure: isInClosure)
    }

    override func visit(_ node: FunctionCallExprSyntax) -> ExprSyntax {
        guard let called = node.calledExpression.as(DeclReferenceExprSyntax.self),
              called.baseName.text == "print" else {
            return super.visit(node)
        }

        let context = findEnclosingContext(startNode: Syntax(node))

        // Only replace print if we are reasonably sure a 'logger' is in scope (i.e., inside a type we modify)
        // Relax the guard: Allow replacement if we found an enclosing type OR if we are inside a Task/closure
        let enclosingTypeFound = context.typeKind != nil && context.typeName != nil
        let isInsideTaskClosure = context.isInClosure
        guard enclosingTypeFound || isInsideTaskClosure else {
            // print("Skipping print replacement outside of a recognized type or Task/closure context.") // DEBUG
            return super.visit(node) // Don't replace if not inside a processed type or detected closure
        }

        // If typeName wasn't found by walking up, but we are in a closure, we might need it for struct/enum prefix.
        // However, assuming Task/closures are primarily used in classes/actors where `self.` is needed anyway.
        let containingTypeName = context.typeName // Might be nil if only isInsideTaskClosure is true

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
            
            var argumentText = ""
            var containsInterpolation = false

            if let stringLiteral = arg.expression.as(StringLiteralExprSyntax.self) {
                for segment in stringLiteral.segments {
                    if let textSegment = segment.as(StringSegmentSyntax.self) {
                        let text = textSegment.content.text
                        argumentText += text
                        // Escape string for use in code
                        var escaped = text
                        escaped = escaped.replacingOccurrences(of: "\\", with: "\\\\")
                        escaped = escaped.replacingOccurrences(of: "\"", with: "\\\"")
                        escaped = escaped.replacingOccurrences(of: "\n", with: "\\n")
                        escaped = escaped.replacingOccurrences(of: "\t", with: "\\t")
                        logMessageCode += escaped
                    } else if let exprSegment = segment.as(ExpressionSegmentSyntax.self),
                              let expr = exprSegment.expressions.first?.expression {
                        let exprText = expr.description
                        argumentText += " \\(\(exprText)) " // Add placeholder for keyword check
                        logMessageCode += "\\(\(exprText))"
                        containsInterpolation = true
                    }
                }
            } else {
                // Handle non-string literal arguments (e.g., variables)
                let exprText = arg.expression.description
                argumentText += " \\(\(exprText)) " // Add placeholder for keyword check
                logMessageCode += "\\(\(exprText))"
                containsInterpolation = true
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
        
        // Determine logger call prefix
        var loggerPrefix = ""

        switch context.typeKind {
        case .structDecl, .enumDecl:
            // Structs and Enums use static logger: TypeName.logger
            // This requires containingTypeName to be non-nil.
            if let name = containingTypeName {
                loggerPrefix = name + "."
            } else {
                // Cannot form TypeName.logger, fallback or skip?
                // For now, assume this case doesn't happen with isInsideTaskClosure = true
                // If it did, we might want to skip the replacement entirely.
                loggerPrefix = "" // Fallback: might be wrong
            }
        case .classDecl, .actorDecl:
            // Classes and Actors use self.logger inside closures OR Task blocks
            if context.isInClosure || isInsideTaskClosure { // Use self. if in a detected closure or Task block
                loggerPrefix = "self."
            } else {
                 // Direct method calls use implicit self
                 loggerPrefix = ""
            }
        default: // Handles case where context.typeKind is nil
             // If we are here because isInsideTaskClosure was true, assume class/actor context and use `self.`
             if isInsideTaskClosure {
                 loggerPrefix = "self."
             } else {
                 // Should not happen based on the guard logic, default to no prefix
                 loggerPrefix = ""
             }
        }

        // Use the determined log level directly as the method name.
        // os.Logger has methods like .warning(), .error(), .info(), etc.
        let loggerMethod = logLevel
        let loggerCallCode = "\(loggerPrefix)logger.\(loggerMethod)(\(logMessageCode))"

        var newCallExpr: ExprSyntax = ExprSyntax("\(raw: loggerCallCode)")
        if let firstToken = node.firstToken(viewMode: .sourceAccurate),
           let lastToken = node.lastToken(viewMode: .sourceAccurate) {
            newCallExpr = newCallExpr
                .with(\.leadingTrivia, firstToken.leadingTrivia)
                .with(\.trailingTrivia, lastToken.trailingTrivia)
        }
        return super.visit(newCallExpr)
    }
    
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
        guard let transformedClass = super.visit(node).as(ClassDeclSyntax.self) else {
            // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
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
        let typeName = transformedClass.name.text

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify it if needed (ONLY subsystem)
            // DO NOT change protection level or add/remove static here
            guard var binding = existingLoggerDecl.bindings.first, // Assume single binding logger = ...
                  let initializer = binding.initializer,
                  var callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil
                self.currentTypeKind = nil
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
                return DeclSyntax(updatedClass)
            } else {
                 // Logger exists and is correct, return unchanged
                self.currentTypeName = nil
                self.currentTypeKind = nil
                return DeclSyntax(transformedClass)
            }

        } else {
            // Logger does not exist, insert a new one: internal let logger = ...
            let loggerDeclCode = "internal let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for class \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
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
                var nextMember = updatedMembers[nextMemberIndex]
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
            return DeclSyntax(updatedClass)
        }
    }

    // --- Add visit for ActorDeclSyntax ---
    override func visit(_ node: ActorDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        guard let transformedActor = super.visit(node).as(ActorDeclSyntax.self) else {
            // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
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
        let typeName = transformedActor.name.text

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify ONLY subsystem if needed
             guard var binding = existingLoggerDecl.bindings.first,
                   let initializer = binding.initializer,
                   var callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
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
                 return DeclSyntax(updatedActor)
             } else {
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 return DeclSyntax(transformedActor)
             }
        } else {
            // Insert new logger: internal let logger = ...
            let loggerDeclCode = "internal let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for actor \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
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
                 var nextMember = updatedMembers[nextMemberIndex]
                 // Adjust for enum cases when determining spacing
                 let requiredNewlines = (nextMember.decl.is(VariableDeclSyntax.self) || nextMember.decl.is(EnumCaseDeclSyntax.self)) ? 1 : 2
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
            return DeclSyntax(updatedActor)
        }
    }

    override func visit(_ node: StructDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        guard let transformedStruct = super.visit(node).as(StructDeclSyntax.self) else {
             // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
            return DeclSyntax(node)
        }

        var loggerVarDecl: VariableDeclSyntax? = nil
        var loggerMemberIndex: Int? = nil
        var firstNonVarDeclIndex: Int? = nil // Find the first func/init/subscript/nested type
        
        // Iterate through members to find existing logger AND first non-var index
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
        var updatedMembers = transformedStruct.memberBlock.members
        let typeName = transformedStruct.name.text

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify ONLY subsystem if needed
            // DO NOT change protection level or add/remove static here
            guard var binding = existingLoggerDecl.bindings.first,
                  let initializer = binding.initializer,
                  var callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil
                self.currentTypeKind = nil
                return DeclSyntax(transformedStruct) // Return unchanged
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
                 // Preserve existing modifiers (static, protection level)
                 let newLoggerDecl = existingLoggerDecl.with(\.bindings, [newBinding])

                 let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                 updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))

                 let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                 let updatedStruct = transformedStruct.with(\.memberBlock, newMemberBlock)
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 return DeclSyntax(updatedStruct)
             } else {
                 // Logger exists and is correct, return unchanged
                 self.currentTypeName = nil
                 self.currentTypeKind = nil
                 return DeclSyntax(transformedStruct)
             }

        } else {
            // Logger does not exist, insert a new one: internal static let logger = ...
            let loggerDeclCode = "internal static let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for struct \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
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
            return DeclSyntax(updatedStruct)
        }
    }

    // --- Add visit for EnumDeclSyntax ---
     override func visit(_ node: EnumDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        self.currentTypeKind = node.kind // Track kind
        guard let transformedEnum = super.visit(node).as(EnumDeclSyntax.self) else {
            // Clear context before returning
            self.currentTypeName = nil
            self.currentTypeKind = nil
            return DeclSyntax(node)
        }

        var loggerVarDecl: VariableDeclSyntax? = nil
        var loggerMemberIndex: Int? = nil
        var firstNonVarDeclIndex: Int? = nil

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

        var updatedMembers = transformedEnum.memberBlock.members
        let typeName = transformedEnum.name.text

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify ONLY subsystem if needed
            guard var binding = existingLoggerDecl.bindings.first,
                  let initializer = binding.initializer,
                  var callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                self.currentTypeName = nil
                self.currentTypeKind = nil
                return DeclSyntax(transformedEnum)
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
                return DeclSyntax(updatedEnum)
            } else {
                self.currentTypeName = nil
                self.currentTypeKind = nil
                return DeclSyntax(transformedEnum)
            }
        } else {
            // Insert new logger: internal static let logger = ...
            let loggerDeclCode = "internal static let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for enum \(typeName)\\n", stderr)
                    self.currentTypeName = nil
                    self.currentTypeKind = nil
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
            return DeclSyntax(updatedEnum)
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
    print("Found Swift file: \(fileURL.path)")
    let sourceFile: SourceFileSyntax
    do {
        // Read the file content
        let sourceCode = try String(contentsOf: fileURL, encoding: .utf8)
        sourceFile = try Parser.parse(source: sourceCode)
        // Print successful parse
//        print("  Successfully parsed.")
    } catch {
        fputs("  Skipping file \(fileURL.path) due to parsing error: \(error)\n", stderr)
        continue
    }

    // Print that the file is being processed
    print("  Processing file...")
    
    let transformedSyntax = rewriter.visit(sourceFile)
    guard let transformedFile = transformedSyntax.as(SourceFileSyntax.self) else {
         fputs("Rewriter failed to return SourceFileSyntax for \(fileURL.path)\n", stderr)
         continue
    }
    
    var finalFile = transformedFile
    var hasImport = false
    for stmt in transformedFile.statements {
        if let importDecl = stmt.item.as(ImportDeclSyntax.self),
           importDecl.description.contains("import os.log") {
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
    } catch {
        fputs("Failed to write \(fileURL.path): \(error)\n", stderr)
    }
    modifiedCount += 1
}

print("Processing complete. \(modifiedCount) file(s) modified.")
