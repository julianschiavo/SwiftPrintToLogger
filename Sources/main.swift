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

    init(moduleName: String) {
        self.moduleName = moduleName
    }

    override func visit(_ node: FunctionCallExprSyntax) -> ExprSyntax {
        guard let called = node.calledExpression.as(DeclReferenceExprSyntax.self),
              called.baseName.text == "print" else {
            return super.visit(node)
        }

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
        
        // Use the determined log level directly as the method name.
        // os.Logger has methods like .warning(), .error(), .info(), etc.
        let loggerMethod = logLevel 
        let loggerCallCode = "logger.\(loggerMethod)(\(logMessageCode))"

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
        guard let transformedClass = super.visit(node).as(ClassDeclSyntax.self) else {
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
                   decl.is(TypeAliasDeclSyntax.self) {
                    firstNonVarDeclIndex = index
                }
            }
        }

        // --- Modify Existing Logger OR Insert New One ---
        var updatedMembers = transformedClass.memberBlock.members
        let typeName = transformedClass.name.text

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify it if needed
            guard var binding = existingLoggerDecl.bindings.first, // Assume single binding logger = ...
                  let initializer = binding.initializer, 
                  var callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil 
                return DeclSyntax(transformedClass) // Return unchanged
            }

            var subsystemArgExists = false
            var needsModification = false
            var updatedArgs = callExpr.arguments

            for (argIndex, arg) in callExpr.arguments.enumerated() {
                if arg.label?.text == "subsystem" {
                    subsystemArgExists = true
                    // Check if the value needs updating - safer comparison
                    if let stringLiteral = arg.expression.as(StringLiteralExprSyntax.self) {
                        if stringLiteral.segments.description != moduleName {
                            needsModification = true
                        }
                    } else {
                        // If it's not a simple string literal, assume modification needed?
                        // Or compare description as fallback? For now, assume non-match.
                        needsModification = true 
                    }
                    
                    if needsModification {
                        let newExpr = StringLiteralExprSyntax(content: moduleName)
                         // Use the correct keypath for the LabeledExprSyntax
                        let updatedArg = arg.with(\.expression, ExprSyntax(newExpr))
                        let listIndex = updatedArgs.index(updatedArgs.startIndex, offsetBy: argIndex)
                        updatedArgs[listIndex] = updatedArg // Directly update the mutable list
                    }
                    break // Found subsystem, no need to check further args
                }
            }

            if !subsystemArgExists {
                // Add the subsystem argument
                let subsystemExpr = LabeledExprSyntax(label: "subsystem",
                                                      colon: .colonToken(trailingTrivia: .space),
                                                      expression: ExprSyntax(StringLiteralExprSyntax(content: moduleName)))
                // Append the new argument, ensuring correct trivia if list wasn't empty
                 if updatedArgs.isEmpty {
                    updatedArgs.append(subsystemExpr)
                } else {
                    // Add comma and space to previous last argument's trailing trivia
                    if var lastArg = updatedArgs.last, let lastIndex = updatedArgs.indices.last {
                       let commaToken = TokenSyntax.commaToken(trailingTrivia: .space)
                       lastArg = lastArg.with(\.trailingComma, commaToken)
                       updatedArgs[lastIndex] = lastArg
                    }
                    updatedArgs.append(subsystemExpr)
                }
                needsModification = true
            }

            if needsModification {
                // Reconstruct the FunctionCallExprSyntax with the updated arguments
                let newCallExpr = FunctionCallExprSyntax(
                    calledExpression: callExpr.calledExpression,
                    leftParen: callExpr.leftParen,
                    arguments: updatedArgs, // Use the modified list
                    rightParen: callExpr.rightParen,
                    trailingClosure: callExpr.trailingClosure,
                    additionalTrailingClosures: callExpr.additionalTrailingClosures
                )
                
                // Reconstruct the InitializerClauseSyntax
                let equalToken = initializer.equal.with(\.leadingTrivia, .space).with(\.trailingTrivia, Trivia())
                let newInitializer = InitializerClauseSyntax(equal: equalToken, value: ExprSyntax(newCallExpr))

                // Reconstruct the PatternBindingSyntax
                let patternWithoutTrivia = binding.pattern.with(\.trailingTrivia, Trivia())
                let newBinding = PatternBindingSyntax(
                    pattern: patternWithoutTrivia,
                    typeAnnotation: binding.typeAnnotation,
                    initializer: newInitializer,
                    accessorBlock: binding.accessorBlock,
                    trailingComma: binding.trailingComma
                )

                // Reconstruct the VariableDeclSyntax, ensuring 'private' for classes
                var finalModifiers = existingLoggerDecl.modifiers
                let privateExists = finalModifiers.contains { $0.name.tokenKind == .keyword(.private) }
                if !privateExists {
                    let privateModifier = DeclModifierSyntax(name: .keyword(.private), trailingTrivia: .space)
                    finalModifiers.insert(privateModifier, at: finalModifiers.startIndex)
                }

                let newLoggerDecl = VariableDeclSyntax(
                    attributes: existingLoggerDecl.attributes,
                    modifiers: finalModifiers,
                    bindingSpecifier: existingLoggerDecl.bindingSpecifier
                ) { 
                    newBinding // Use the new binding
                }

                // Replace the old member in the list
                let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))
                
                // Reconstruct MemberBlock and ClassDecl
                 let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                 let updatedClass = transformedClass.with(\.memberBlock, newMemberBlock)
                 self.currentTypeName = nil
                return DeclSyntax(updatedClass)
            } else {
                 // Logger exists and is correct, return unchanged
                self.currentTypeName = nil
                return DeclSyntax(transformedClass)
            }

        } else {
            // Logger does not exist, insert a new one
            let loggerDeclCode = "private let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for \(typeName)\n", stderr)
                    self.currentTypeName = nil
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
            return DeclSyntax(updatedClass)
        }
    }

    override func visit(_ node: StructDeclSyntax) -> DeclSyntax {
        self.currentTypeName = node.name.text
        guard let transformedStruct = super.visit(node).as(StructDeclSyntax.self) else {
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
                   decl.is(TypeAliasDeclSyntax.self) {
                    firstNonVarDeclIndex = index
                }
            }
        }

        // --- Modify Existing Logger OR Insert New One ---
        var updatedMembers = transformedStruct.memberBlock.members
        let typeName = transformedStruct.name.text

        if let existingLoggerDecl = loggerVarDecl, let index = loggerMemberIndex {
            // Logger exists, modify it if needed
            guard var binding = existingLoggerDecl.bindings.first, // Assume single binding logger = ...
                  let initializer = binding.initializer, 
                  var callExpr = initializer.value.as(FunctionCallExprSyntax.self) else {
                // Malformed logger declaration, skip modification
                self.currentTypeName = nil 
                return DeclSyntax(transformedStruct) // Return unchanged
            }

            var subsystemArgExists = false
            var needsModification = false
            var updatedArgs = callExpr.arguments

            for (argIndex, arg) in callExpr.arguments.enumerated() {
                if arg.label?.text == "subsystem" {
                    subsystemArgExists = true
                    // Check if the value needs updating - safer comparison
                    if let stringLiteral = arg.expression.as(StringLiteralExprSyntax.self) {
                        if stringLiteral.segments.description != moduleName {
                            needsModification = true
                        }
                    } else {
                        needsModification = true 
                    }
                    
                    if needsModification {
                        let newExpr = StringLiteralExprSyntax(content: moduleName)
                         // Use the correct keypath for the LabeledExprSyntax
                        let updatedArg = arg.with(\.expression, ExprSyntax(newExpr))
                        let listIndex = updatedArgs.index(updatedArgs.startIndex, offsetBy: argIndex)
                        updatedArgs[listIndex] = updatedArg // Directly update the mutable list
                    }
                    break // Found subsystem, no need to check further args
                }
            }

            if !subsystemArgExists {
                // Add the subsystem argument
                let subsystemExpr = LabeledExprSyntax(label: "subsystem",
                                                      colon: .colonToken(trailingTrivia: .space),
                                                      expression: ExprSyntax(StringLiteralExprSyntax(content: moduleName)))
                // Append the new argument, ensuring correct trivia if list wasn't empty
                 if updatedArgs.isEmpty {
                    updatedArgs.append(subsystemExpr)
                } else {
                    // Add comma and space to previous last argument's trailing trivia
                    if var lastArg = updatedArgs.last, let lastIndex = updatedArgs.indices.last {
                       let commaToken = TokenSyntax.commaToken(trailingTrivia: .space)
                       lastArg = lastArg.with(\.trailingComma, commaToken)
                       updatedArgs[lastIndex] = lastArg
                    }
                    updatedArgs.append(subsystemExpr)
                }
                needsModification = true
            }

            if needsModification {
                // Reconstruct the FunctionCallExprSyntax with the updated arguments
                let newCallExpr = FunctionCallExprSyntax(
                    calledExpression: callExpr.calledExpression,
                    leftParen: callExpr.leftParen,
                    arguments: updatedArgs, // Use the modified list
                    rightParen: callExpr.rightParen,
                    trailingClosure: callExpr.trailingClosure,
                    additionalTrailingClosures: callExpr.additionalTrailingClosures
                )
                
                // Reconstruct the InitializerClauseSyntax
                let equalToken = initializer.equal.with(\.leadingTrivia, .space).with(\.trailingTrivia, Trivia())
                let newInitializer = InitializerClauseSyntax(equal: equalToken, value: ExprSyntax(newCallExpr))

                // Reconstruct the PatternBindingSyntax
                let patternWithoutTrivia = binding.pattern.with(\.trailingTrivia, Trivia())
                let newBinding = PatternBindingSyntax(
                    pattern: patternWithoutTrivia,
                    typeAnnotation: binding.typeAnnotation,
                    initializer: newInitializer,
                    accessorBlock: binding.accessorBlock,
                    trailingComma: binding.trailingComma
                )

                // Reconstruct the VariableDeclSyntax (no private for structs)
                let newLoggerDecl = VariableDeclSyntax(
                    attributes: existingLoggerDecl.attributes,
                    modifiers: existingLoggerDecl.modifiers,
                    bindingSpecifier: existingLoggerDecl.bindingSpecifier
                ) { 
                    newBinding // Use the new binding
                }

                // Replace the old member in the list
                let memberIndex = updatedMembers.index(updatedMembers.startIndex, offsetBy: index)
                updatedMembers[memberIndex] = MemberBlockItemSyntax(decl: DeclSyntax(newLoggerDecl))
                
                // Reconstruct MemberBlock and StructDecl
                 let newMemberBlock = MemberBlockSyntax(members: updatedMembers)
                 let updatedStruct = transformedStruct.with(\.memberBlock, newMemberBlock)
                 self.currentTypeName = nil
                return DeclSyntax(updatedStruct)
            } else {
                 // Logger exists and is correct, return unchanged
                self.currentTypeName = nil
                return DeclSyntax(transformedStruct)
            }

        } else {
            // Logger does not exist, insert a new one
            // Structs get non-private logger
            let loggerDeclCode = "let logger = Logger(subsystem: \"\(moduleName)\", category: \"\(typeName)\")"
            let loggerMember: MemberBlockItemSyntax
            do {
                guard let stmt = try? Parser.parse(source: loggerDeclCode).statements.first,
                      let varDecl = stmt.item.as(VariableDeclSyntax.self) else {
                    fputs("Error parsing logger variable declaration for \(typeName)\n", stderr)
                    self.currentTypeName = nil
                    return DeclSyntax(transformedStruct) // Return unchanged on error
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

            let newMemberBlock = transformedStruct.memberBlock.with(\.members, updatedMembers)
            let updatedStruct = transformedStruct.with(\.memberBlock, newMemberBlock)
            self.currentTypeName = nil
            return DeclSyntax(updatedStruct)
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
        importItem = importItem.with(\.trailingTrivia, Trivia.newlines(1))
        
        let newStatements = [importItem] + finalFile.statements
        finalFile = finalFile.with(\.statements, newStatements)
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
