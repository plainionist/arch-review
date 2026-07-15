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
    let (SynTypeDefn(typeInfo, typeRepr, _, _, _, _)) = typeDefn
    let (SynComponentInfo(_, _, _, longId, _, _, _, _)) = typeInfo
    let localName = joinIdentifiers longId

    let fullName =
        match currentModule with
        | Some moduleName when not (String.IsNullOrWhiteSpace(moduleName)) -> $"{moduleName}.{localName}"
        | _ -> localName

    {
        projectPath = projectPath
        filePath = filePath
        moduleFullName = currentModule
        fullName = fullName
        name = localName
        kind = typeKindFromRepr typeRepr
        referencedTypeNames = collectReferencedTypeNames typeRepr
    }

let extractFromParseTree (projectPath: string) (filePath: string) (parseTree: ParsedInput) =
    let modules = ResizeArray<RawModule>()
    let types = ResizeArray<RawType>()
    let moduleUses = ResizeArray<RawDependency>()

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
        warnings = []
    }
