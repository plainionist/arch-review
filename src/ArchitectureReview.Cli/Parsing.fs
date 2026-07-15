module ArchitectureReview.Parsing

open System
open System.IO
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open ArchitectureReview.Common
open ArchitectureReview.Model

let joinIdentifiers (idents: Ident list) =
    idents |> List.map (fun i -> i.idText) |> String.concat "."

let typeKindFromRepr (repr: SynTypeDefnRepr) =
    match repr with
    | SynTypeDefnRepr.Simple(simpleRepr, _) ->
        match simpleRepr with
        | SynTypeDefnSimpleRepr.Record _ -> "record"
        | SynTypeDefnSimpleRepr.Union _ -> "discriminated-union"
        | SynTypeDefnSimpleRepr.Enum _ -> "enum"
        | SynTypeDefnSimpleRepr.TypeAbbrev _ -> "type-alias"
        | SynTypeDefnSimpleRepr.General _ -> "type"
        | SynTypeDefnSimpleRepr.None _ -> "type"
        | SynTypeDefnSimpleRepr.Exception _ -> "exception"
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> "type"
    | SynTypeDefnRepr.ObjectModel _ -> "object-model"
    | SynTypeDefnRepr.Exception _ -> "exception"

let rec collectTypeNamesFromSynType (synType: SynType) =
    match synType with
    | SynType.LongIdent(SynLongIdent(id, _, _)) -> [ joinIdentifiers id ]
    | SynType.App(typeName, _, typeArgs, _, _, _, _) ->
        (collectTypeNamesFromSynType typeName)
        @ (typeArgs |> List.collect collectTypeNamesFromSynType)
    | SynType.LongIdentApp(typeName, SynLongIdent(id, _, _), _, typeArgs, _, _, _) ->
        [ joinIdentifiers id ]
        @ (collectTypeNamesFromSynType typeName)
        @ (typeArgs |> List.collect collectTypeNamesFromSynType)
    | SynType.Array(_, elementType, _) -> collectTypeNamesFromSynType elementType
    | SynType.Fun(argType, returnType, _, _) ->
        (collectTypeNamesFromSynType argType)
        @ (collectTypeNamesFromSynType returnType)
    | SynType.WithGlobalConstraints(typeName, _, _) -> collectTypeNamesFromSynType typeName
    | SynType.HashConstraint(innerType, _) -> collectTypeNamesFromSynType innerType
    | SynType.MeasurePower(baseMeasure, _, _) -> collectTypeNamesFromSynType baseMeasure
    | SynType.WithNull(innerType, _, _, _) -> collectTypeNamesFromSynType innerType
    | SynType.Paren(innerType, _) -> collectTypeNamesFromSynType innerType
    | SynType.SignatureParameter(_, _, _, usedType, _) -> collectTypeNamesFromSynType usedType
    | SynType.Or(lhsType, rhsType, _, _) ->
        (collectTypeNamesFromSynType lhsType)
        @ (collectTypeNamesFromSynType rhsType)
    | SynType.Intersection(_, types, _, _) -> types |> List.collect collectTypeNamesFromSynType
    | _ -> []

let collectTypesFromUnionCaseKind (caseKind: SynUnionCaseKind) =
    match caseKind with
    | SynUnionCaseKind.Fields(fields) ->
        fields
        |> List.collect (fun (SynField(_, _, _, fieldType, _, _, _, _, _)) -> collectTypeNamesFromSynType fieldType)
    | SynUnionCaseKind.FullType(fullType, _) -> collectTypeNamesFromSynType fullType

let collectTypesFromSimpleRepr (repr: SynTypeDefnSimpleRepr) =
    match repr with
    | SynTypeDefnSimpleRepr.Record(_, recordFields, _) ->
        recordFields
        |> List.collect (fun (SynField(_, _, _, fieldType, _, _, _, _, _)) -> collectTypeNamesFromSynType fieldType)
    | SynTypeDefnSimpleRepr.Union(_, unionCases, _) ->
        unionCases
        |> List.collect (fun (SynUnionCase(_, _, caseType, _, _, _, _)) -> collectTypesFromUnionCaseKind caseType)
    | SynTypeDefnSimpleRepr.TypeAbbrev(_, rhsType, _) -> collectTypeNamesFromSynType rhsType
    | _ -> []

let collectReferencedTypeNames (repr: SynTypeDefnRepr) =
    match repr with
    | SynTypeDefnRepr.Simple(simpleRepr, _) -> collectTypesFromSimpleRepr simpleRepr
    | _ -> []

let rec collectTypeNamesFromPattern (pat: SynPat) =
    match pat with
    | SynPat.Typed(innerPat, patType, _) ->
        (collectTypeNamesFromPattern innerPat) @ (collectTypeNamesFromSynType patType)
    | SynPat.Attrib(innerPat, _, _) -> collectTypeNamesFromPattern innerPat
    | SynPat.Or(leftPat, rightPat, _, _) ->
        (collectTypeNamesFromPattern leftPat) @ (collectTypeNamesFromPattern rightPat)
    | SynPat.Ands(patterns, _) -> patterns |> List.collect collectTypeNamesFromPattern
    | SynPat.Paren(innerPat, _) -> collectTypeNamesFromPattern innerPat
    | SynPat.Tuple(_, patterns, _, _) -> patterns |> List.collect collectTypeNamesFromPattern
    | _ -> []

let rec collectTypeNamesFromExpr (expr: SynExpr) =
    match expr with
    | SynExpr.LongIdent(_, SynLongIdent(id, _, _), _, _) -> [ joinIdentifiers id ]
    | SynExpr.Typed(innerExpr, typedAs, _) ->
        (collectTypeNamesFromExpr innerExpr) @ (collectTypeNamesFromSynType typedAs)
    | SynExpr.New(_, newType, ctorExpr, _) ->
        (collectTypeNamesFromSynType newType) @ (collectTypeNamesFromExpr ctorExpr)
    | SynExpr.Upcast(innerExpr, targetType, _) ->
        (collectTypeNamesFromExpr innerExpr) @ (collectTypeNamesFromSynType targetType)
    | SynExpr.Downcast(innerExpr, targetType, _) ->
        (collectTypeNamesFromExpr innerExpr) @ (collectTypeNamesFromSynType targetType)
    | SynExpr.TypeApp(innerExpr, _, typeArgs, _, _, _, _) ->
        (collectTypeNamesFromExpr innerExpr) @ (typeArgs |> List.collect collectTypeNamesFromSynType)
    | SynExpr.App(_, _, funcExpr, argExpr, _) ->
        (collectTypeNamesFromExpr funcExpr) @ (collectTypeNamesFromExpr argExpr)
    | SynExpr.LetOrUse(_, _, _, _, bindings, bodyExpr, _, _) ->
        (bindings |> List.collect collectTypeNamesFromBinding)
        @ (collectTypeNamesFromExpr bodyExpr)
    | SynExpr.Lambda(_, _, _, bodyExpr, _, _, _) -> collectTypeNamesFromExpr bodyExpr
    | SynExpr.Sequential(_, _, firstExpr, secondExpr, _, _) ->
        (collectTypeNamesFromExpr firstExpr) @ (collectTypeNamesFromExpr secondExpr)
    | SynExpr.IfThenElse(condExpr, thenExpr, elseExprOpt, _, _, _, _) ->
        (collectTypeNamesFromExpr condExpr)
        @ (collectTypeNamesFromExpr thenExpr)
        @ (elseExprOpt |> Option.map collectTypeNamesFromExpr |> Option.defaultValue [])
    | SynExpr.Paren(innerExpr, _, _, _) -> collectTypeNamesFromExpr innerExpr
    | SynExpr.Tuple(_, exprs, _, _) -> exprs |> List.collect collectTypeNamesFromExpr
    | SynExpr.ArrayOrList(_, exprs, _) -> exprs |> List.collect collectTypeNamesFromExpr
    | SynExpr.Match(_, matchExpr, clauses, _, _) ->
        (collectTypeNamesFromExpr matchExpr)
        @ (clauses |> List.collect (fun (SynMatchClause(clausePat, _, clauseExpr, _, _, _)) ->
            (collectTypeNamesFromPattern clausePat) @ (collectTypeNamesFromExpr clauseExpr)))
    | _ -> []

and collectTypeNamesFromBinding (binding: SynBinding) =
    let (SynBinding(_, _, _, _, _, _, _, pat, returnInfo, expr, _, _, _)) = binding

    let returnTypes =
        match returnInfo with
        | Some(SynBindingReturnInfo(returnType, _, _, _)) -> collectTypeNamesFromSynType returnType
        | None -> []

    (collectTypeNamesFromPattern pat)
    @ returnTypes
    @ (collectTypeNamesFromExpr expr)

let rec collectModuleNamesFromExpr (expr: SynExpr) =
    match expr with
    | SynExpr.LongIdent(_, SynLongIdent(id, _, _), _, _) when id.Length >= 2 -> [ joinIdentifiers id ]
    | SynExpr.Typed(innerExpr, _, _) -> collectModuleNamesFromExpr innerExpr
    | SynExpr.New(_, _, ctorExpr, _) -> collectModuleNamesFromExpr ctorExpr
    | SynExpr.Upcast(innerExpr, _, _) -> collectModuleNamesFromExpr innerExpr
    | SynExpr.Downcast(innerExpr, _, _) -> collectModuleNamesFromExpr innerExpr
    | SynExpr.TypeApp(innerExpr, _, _, _, _, _, _) -> collectModuleNamesFromExpr innerExpr
    | SynExpr.App(_, _, funcExpr, argExpr, _) ->
        (collectModuleNamesFromExpr funcExpr) @ (collectModuleNamesFromExpr argExpr)
    | SynExpr.LetOrUse(_, _, _, _, bindings, bodyExpr, _, _) ->
        (bindings |> List.collect collectModuleNamesFromBinding)
        @ (collectModuleNamesFromExpr bodyExpr)
    | SynExpr.Lambda(_, _, _, bodyExpr, _, _, _) -> collectModuleNamesFromExpr bodyExpr
    | SynExpr.Sequential(_, _, firstExpr, secondExpr, _, _) ->
        (collectModuleNamesFromExpr firstExpr) @ (collectModuleNamesFromExpr secondExpr)
    | SynExpr.IfThenElse(condExpr, thenExpr, elseExprOpt, _, _, _, _) ->
        (collectModuleNamesFromExpr condExpr)
        @ (collectModuleNamesFromExpr thenExpr)
        @ (elseExprOpt |> Option.map collectModuleNamesFromExpr |> Option.defaultValue [])
    | SynExpr.Paren(innerExpr, _, _, _) -> collectModuleNamesFromExpr innerExpr
    | SynExpr.Tuple(_, exprs, _, _) -> exprs |> List.collect collectModuleNamesFromExpr
    | SynExpr.ArrayOrList(_, exprs, _) -> exprs |> List.collect collectModuleNamesFromExpr
    | SynExpr.Match(_, matchExpr, clauses, _, _) ->
        (collectModuleNamesFromExpr matchExpr)
        @ (clauses |> List.collect (fun (SynMatchClause(_, _, clauseExpr, _, _, _)) -> collectModuleNamesFromExpr clauseExpr))
    | _ -> []

and collectModuleNamesFromBinding (binding: SynBinding) =
    let (SynBinding(_, _, _, _, _, _, _, _, _, expr, _, _, _)) = binding
    collectModuleNamesFromExpr expr

let rec collectTypeNamesFromMemberDefn (memberDefn: SynMemberDefn) =
    match memberDefn with
    | SynMemberDefn.Member(binding, _) -> collectTypeNamesFromBinding binding
    | SynMemberDefn.GetSetMember(getBindingOpt, setBindingOpt, _, _) ->
        (getBindingOpt |> Option.map collectTypeNamesFromBinding |> Option.defaultValue [])
        @ (setBindingOpt |> Option.map collectTypeNamesFromBinding |> Option.defaultValue [])
    | SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _, _, _) -> collectTypeNamesFromPattern ctorArgs
    | SynMemberDefn.LetBindings(bindings, _, _, _) -> bindings |> List.collect collectTypeNamesFromBinding
    | SynMemberDefn.Interface(_, _, memberDefnsOpt, _) ->
        memberDefnsOpt
        |> Option.map (List.collect collectTypeNamesFromMemberDefn)
        |> Option.defaultValue []
    | SynMemberDefn.NestedType(typeDefn, _, _) ->
        let (SynTypeDefn(_, _, nestedMembers, _, _, _)) = typeDefn
        nestedMembers |> List.collect collectTypeNamesFromMemberDefn
    | _ -> []

let rec collectModuleNamesFromMemberDefn (memberDefn: SynMemberDefn) =
    match memberDefn with
    | SynMemberDefn.Member(binding, _) -> collectModuleNamesFromBinding binding
    | SynMemberDefn.GetSetMember(getBindingOpt, setBindingOpt, _, _) ->
        (getBindingOpt |> Option.map collectModuleNamesFromBinding |> Option.defaultValue [])
        @ (setBindingOpt |> Option.map collectModuleNamesFromBinding |> Option.defaultValue [])
    | SynMemberDefn.LetBindings(bindings, _, _, _) -> bindings |> List.collect collectModuleNamesFromBinding
    | SynMemberDefn.Interface(_, _, memberDefnsOpt, _) ->
        memberDefnsOpt
        |> Option.map (List.collect collectModuleNamesFromMemberDefn)
        |> Option.defaultValue []
    | SynMemberDefn.NestedType(typeDefn, _, _) ->
        let (SynTypeDefn(_, _, nestedMembers, _, _, _)) = typeDefn
        nestedMembers |> List.collect collectModuleNamesFromMemberDefn
    | _ -> []

let collectModuleNamesFromMemberDefns (memberDefns: SynMemberDefn list) =
    memberDefns |> List.collect collectModuleNamesFromMemberDefn

let collectTypeNamesFromMemberDefns (memberDefns: SynMemberDefn list) =
    memberDefns |> List.collect collectTypeNamesFromMemberDefn

let parseFsproj (projectPath: string) =
    let projectPath = normalizePath projectPath
    let projectDir = Path.GetDirectoryName(projectPath)
    let doc = XDocument.Load(projectPath)

    let descendants localName =
        doc.Descendants() |> Seq.filter (fun e -> e.Name.LocalName = localName)

    let resolveInclude includeValue =
        normalizePath (Path.Combine(projectDir, includeValue))

    let compileFiles =
        descendants "Compile"
        |> Seq.choose (fun e ->
            let attr = e.Attribute(XName.Get("Include"))
            if isNull attr || String.IsNullOrWhiteSpace(attr.Value) then None
            else Some(resolveInclude attr.Value))
        |> Seq.filter File.Exists
        |> Seq.toList

    let projectReferences =
        descendants "ProjectReference"
        |> Seq.choose (fun e ->
            let attr = e.Attribute(XName.Get("Include"))
            if isNull attr || String.IsNullOrWhiteSpace(attr.Value) then None
            else Some(resolveInclude attr.Value))
        |> Seq.toList

    {
        projectPath = projectPath
        projectName = Path.GetFileNameWithoutExtension(projectPath)
        compileFiles = compileFiles
        projectReferences = projectReferences
    }

let discoverProjects (rootPath: string) =
    Directory.GetFiles(rootPath, "*.fsproj", SearchOption.AllDirectories)
    |> Array.map normalizePath
    |> Array.sort
    |> Array.toList

let parseFile (checker: FSharpChecker) (parsingOptions: FSharpParsingOptions) (filePath: string) =
    let source = File.ReadAllText(filePath) |> SourceText.ofString
    checker.ParseFile(filePath, source, parsingOptions)
    |> Async.RunSynchronously

let parseTypeDefn (projectPath: string) (filePath: string) (currentModule: string option) (typeDefn: SynTypeDefn) =
    let (SynTypeDefn(typeInfo, typeRepr, members, _, _, _)) = typeDefn
    let (SynComponentInfo(_, _, _, longId, _, _, _, _)) = typeInfo
    let localName = joinIdentifiers longId

    let fullName =
        match currentModule with
        | Some moduleName when not (String.IsNullOrWhiteSpace(moduleName)) -> $"{moduleName}.{localName}"
        | _ -> localName

    let objectModelMemberTypeNames =
        match typeRepr with
        | SynTypeDefnRepr.ObjectModel(_, objectMembers, _) -> collectTypeNamesFromMemberDefns objectMembers
        | _ -> []

    let referencedTypeNames =
        (collectReferencedTypeNames typeRepr)
        @ objectModelMemberTypeNames
        @ (collectTypeNamesFromMemberDefns members)
        |> List.distinct

    {
        projectPath = projectPath
        filePath = filePath
        moduleFullName = currentModule
        fullName = fullName
        name = localName
        kind = typeKindFromRepr typeRepr
        referencedTypeNames = referencedTypeNames
    }

let extractFromParseTree (projectPath: string) (filePath: string) (parseTree: ParsedInput) =
    let modules = ResizeArray<RawModule>()
    let types = ResizeArray<RawType>()
    let moduleUses = ResizeArray<RawDependency>()
    let moduleTypeUses = ResizeArray<RawDependency>()

    let rec walkDecls (currentModule: string option) (decls: SynModuleDecl list) =
        for decl in decls do
            match decl with
            | SynModuleDecl.Open(target, _) ->
                match currentModule, target with
                | Some sourceModule, SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(id, _, _), _) ->
                    moduleUses.Add({ source = sourceModule; target = joinIdentifiers id; details = "open" })
                | _ -> ()
            | SynModuleDecl.NestedModule(moduleInfo, _, nestedDecls, _, _, _) ->
                let (SynComponentInfo(_, _, _, longId, _, _, _, _)) = moduleInfo
                let nestedName = joinIdentifiers longId
                let fullName =
                    match currentModule with
                    | Some parent when not (String.IsNullOrWhiteSpace(parent)) -> $"{parent}.{nestedName}"
                    | _ -> nestedName

                modules.Add({ projectPath = projectPath; filePath = filePath; fullName = fullName })
                walkDecls (Some fullName) nestedDecls
            | SynModuleDecl.Types(typeDefns, _) ->
                for typeDefn in typeDefns do
                    types.Add(parseTypeDefn projectPath filePath currentModule typeDefn)

                    let (SynTypeDefn(_, typeRepr, members, _, _, _)) = typeDefn
                    let objectModelMembers =
                        match typeRepr with
                        | SynTypeDefnRepr.ObjectModel(_, objectMembers, _) -> objectMembers
                        | _ -> []

                    let usedModuleNames =
                        (collectModuleNamesFromMemberDefns objectModelMembers)
                        @ (collectModuleNamesFromMemberDefns members)
                        |> List.distinct

                    match currentModule with
                    | Some sourceModule ->
                        for moduleName in usedModuleNames do
                            moduleUses.Add({ source = sourceModule; target = moduleName; details = "function-call" })
                    | None -> ()
            | SynModuleDecl.Let(_, bindings, _) ->
                for binding in bindings do
                    let usedTypeNames = collectTypeNamesFromBinding binding
                    let usedModuleNames = collectModuleNamesFromBinding binding
                    match currentModule with
                    | Some sourceModule ->
                        for typeName in usedTypeNames do
                            moduleTypeUses.Add({ source = sourceModule; target = typeName; details = "binding-type" })
                        for moduleName in usedModuleNames do
                            moduleUses.Add({ source = sourceModule; target = moduleName; details = "function-call" })
                    | None -> ()
            | SynModuleDecl.Expr(expr, _) ->
                let usedTypeNames = collectTypeNamesFromExpr expr
                let usedModuleNames = collectModuleNamesFromExpr expr
                match currentModule with
                | Some sourceModule ->
                    for typeName in usedTypeNames do
                        moduleTypeUses.Add({ source = sourceModule; target = typeName; details = "expr-type" })
                    for moduleName in usedModuleNames do
                        moduleUses.Add({ source = sourceModule; target = moduleName; details = "function-call" })
                | None -> ()
            | SynModuleDecl.NamespaceFragment fragment ->
                walkModuleOrNamespace fragment
            | _ -> ()

    and walkModuleOrNamespace (synModule: SynModuleOrNamespace) =
        let (SynModuleOrNamespace(longId, _, _, decls, _, _, _, _, _)) = synModule
        let moduleName = joinIdentifiers longId
        modules.Add({ projectPath = projectPath; filePath = filePath; fullName = moduleName })
        walkDecls (Some moduleName) decls

    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, contents, _, _, _)) ->
        contents |> List.iter walkModuleOrNamespace
    | _ -> ()

    {
        modules = modules |> Seq.distinctBy (fun m -> m.projectPath, m.filePath, m.fullName) |> Seq.toList
        types = types |> Seq.distinctBy (fun t -> t.projectPath, t.filePath, t.fullName) |> Seq.toList
        moduleUses = moduleUses |> Seq.distinctBy (fun d -> d.source, d.target, d.details) |> Seq.toList
        moduleTypeUses = moduleTypeUses |> Seq.distinctBy (fun d -> d.source, d.target, d.details) |> Seq.toList
        warnings = []
    }
