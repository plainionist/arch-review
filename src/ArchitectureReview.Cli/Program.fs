open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type ProjectNode = {
    id: string
    name: string
    path: string
    folder: string
    fileIds: string list
}

type FolderNode = {
    id: string
    projectId: string
    path: string
}

type FileNode = {
    id: string
    projectId: string
    folderId: string
    path: string
    compileOrder: int
}

type ModuleNode = {
    id: string
    projectId: string
    fileId: string
    name: string
    fullName: string
}

type TypeNode = {
    id: string
    projectId: string
    fileId: string
    moduleId: string option
    name: string
    fullName: string
    kind: string
}

type Edge = {
    sourceId: string
    targetId: string
    kind: string
    details: string
}

type ArchitectureModel = {
    schemaVersion: string
    generatedAtUtc: string
    rootPath: string
    projects: ProjectNode list
    folders: FolderNode list
    files: FileNode list
    modules: ModuleNode list
    types: TypeNode list
    edges: Edge list
    warnings: string list
}

type ParsedProject = {
    projectPath: string
    projectName: string
    compileFiles: string list
    projectReferences: string list
}

type RawModule = {
    projectPath: string
    filePath: string
    fullName: string
}

type RawType = {
    projectPath: string
    filePath: string
    moduleFullName: string option
    fullName: string
    name: string
    kind: string
    referencedTypeNames: string list
}

type RawDependency = {
    source: string
    target: string
    details: string
}

type ExtractionResult = {
    modules: RawModule list
    types: RawType list
    moduleUses: RawDependency list
    warnings: string list
}

type CliOptions = {
    targetFolder: string
    outputFolder: string option
    focusSymbol: string option
}

let normalizePath (path: string) =
    Path.GetFullPath(path).Replace('\\', '/')

let relPath (rootPath: string) (path: string) =
    Path.GetRelativePath(rootPath, path).Replace('\\', '/')

let shaShort (input: string) =
    use sha = SHA256.Create()
    let bytes = Encoding.UTF8.GetBytes(input)
    let hash = sha.ComputeHash(bytes)
    hash
    |> Seq.take 8
    |> Seq.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let makeId (prefix: string) (value: string) =
    $"{prefix}_{shaShort value}"

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

let buildModel (rootPath: string) =
    let rootPath = normalizePath rootPath
    let discoveredProjects = discoverProjects rootPath
    let parsedProjects = discoveredProjects |> List.map parseFsproj
    let checker = FSharpChecker.Create(projectCacheSize = 100)
    let warnings = ResizeArray<string>()

    let projectIds =
        parsedProjects
        |> List.map (fun p -> p.projectPath, makeId "project" p.projectPath)
        |> Map.ofList

    let knownProjects = projectIds |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    let files =
        parsedProjects
        |> List.collect (fun p ->
            let pid = projectIds[p.projectPath]
            p.compileFiles
            |> List.mapi (fun idx filePath ->
                let folderPath = normalizePath (Path.GetDirectoryName(filePath))
                {
                    id = makeId "file" filePath
                    projectId = pid
                    folderId = makeId "folder" (pid + "|" + folderPath)
                    path = relPath rootPath filePath
                    compileOrder = idx
                }))

    let folders =
        files
        |> Seq.map (fun file ->
            let fileAbs = normalizePath (Path.Combine(rootPath, file.path))
            let folderAbs = normalizePath (Path.GetDirectoryName(fileAbs))
            {
                id = file.folderId
                projectId = file.projectId
                path = relPath rootPath folderAbs
            })
        |> Seq.distinctBy (fun f -> f.id)
        |> Seq.sortBy (fun f -> f.path)
        |> Seq.toList

    let projects =
        parsedProjects
        |> List.map (fun p ->
            let pid = projectIds[p.projectPath]
            let projectFileIds = p.compileFiles |> List.map (makeId "file")
            {
                id = pid
                name = p.projectName
                path = relPath rootPath p.projectPath
                folder = relPath rootPath (Path.GetDirectoryName(p.projectPath))
                fileIds = projectFileIds
            })
        |> List.sortBy (fun p -> p.path)

    let extraction =
        parsedProjects
        |> List.collect (fun p ->
            let parsingOptions =
                { FSharpParsingOptions.Default with
                    SourceFiles = p.compileFiles |> List.toArray
                    IsExe = true
                    IsInteractive = false }

            p.compileFiles
            |> List.collect (fun filePath ->
                try
                    let parseResult = parseFile checker parsingOptions filePath
                    let parseWarnings =
                        parseResult.Diagnostics
                        |> Seq.filter (fun d -> String.Equals(d.Severity.ToString(), "Warning", StringComparison.OrdinalIgnoreCase))
                        |> Seq.map (fun d -> $"{relPath rootPath filePath} ({d.StartLine}:{d.StartColumn}) {d.Message}")
                        |> Seq.toList

                    let parseErrors =
                        parseResult.Diagnostics
                        |> Seq.filter (fun d -> String.Equals(d.Severity.ToString(), "Error", StringComparison.OrdinalIgnoreCase))
                        |> Seq.map (fun d -> $"{relPath rootPath filePath} ({d.StartLine}:{d.StartColumn}) {d.Message}")
                        |> Seq.toList

                    parseWarnings |> List.iter warnings.Add
                    parseErrors |> List.iter warnings.Add

                    let extracted = extractFromParseTree p.projectPath filePath parseResult.ParseTree
                    [ extracted ]
                with ex ->
                    warnings.Add($"Failed to parse {relPath rootPath filePath}: {ex.Message}")
                    []))

    let rawModules = extraction |> List.collect (fun e -> e.modules)
    let rawTypes = extraction |> List.collect (fun e -> e.types)
    let rawModuleUses = extraction |> List.collect (fun e -> e.moduleUses)

    let modules =
        rawModules
        |> Seq.map (fun m ->
            let pid = projectIds[m.projectPath]
            let fileId = makeId "file" m.filePath
            {
                id = makeId "module" (m.projectPath + "|" + m.filePath + "|" + m.fullName)
                projectId = pid
                fileId = fileId
                name = m.fullName.Split('.') |> Array.last
                fullName = m.fullName
            })
        |> Seq.distinctBy (fun m -> m.id)
        |> Seq.sortBy (fun m -> m.fullName)
        |> Seq.toList

    let moduleByQualifiedName =
        modules
        |> Seq.groupBy (fun m -> m.projectId, m.fullName)
        |> Seq.map (fun (k, v) -> k, v |> Seq.sortBy (fun m -> m.fileId) |> Seq.head)
        |> Map.ofSeq

    let moduleIdByProjectAndName =
        modules
        |> Seq.groupBy (fun m -> m.projectId, m.fullName)
        |> Seq.map (fun (k, v) -> k, (v |> Seq.head).id)
        |> Map.ofSeq

    let types =
        rawTypes
        |> Seq.map (fun t ->
            let pid = projectIds[t.projectPath]
            let fileId = makeId "file" t.filePath
            let moduleId =
                t.moduleFullName
                |> Option.bind (fun name -> Map.tryFind (pid, name) moduleIdByProjectAndName)

            {
                id = makeId "type" (t.projectPath + "|" + t.filePath + "|" + t.fullName)
                projectId = pid
                fileId = fileId
                moduleId = moduleId
                name = t.name
                fullName = t.fullName
                kind = t.kind
            })
        |> Seq.distinctBy (fun t -> t.id)
        |> Seq.sortBy (fun t -> t.fullName)
        |> Seq.toList

    let typesByProjectAndName =
        types
        |> Seq.groupBy (fun t -> t.projectId, t.fullName)
        |> Seq.map (fun (k, v) -> k, v |> Seq.head)
        |> Map.ofSeq

    let typesByProjectAndLeafName =
        types
        |> Seq.groupBy (fun t -> t.projectId, t.name)
        |> Seq.map (fun (k, v) -> k, v |> Seq.toList)
        |> Map.ofSeq

    let projectReferenceEdges =
        parsedProjects
        |> List.collect (fun p ->
            let sourceId = projectIds[p.projectPath]
            p.projectReferences
            |> List.choose (fun projectRef ->
                if Set.contains projectRef knownProjects then
                    let targetId = projectIds[projectRef]
                    Some {
                        sourceId = sourceId
                        targetId = targetId
                        kind = "project-reference"
                        details = "ProjectReference"
                    }
                else
                    warnings.Add($"External or missing project reference from {relPath rootPath p.projectPath}: {projectRef}")
                    None))

    let compileOrderEdges =
        parsedProjects
        |> List.collect (fun p ->
            p.compileFiles
            |> List.pairwise
            |> List.map (fun (leftFile, rightFile) ->
                {
                    sourceId = makeId "file" leftFile
                    targetId = makeId "file" rightFile
                    kind = "compile-order"
                    details = p.projectName
                }))

    let moduleUseEdges =
        rawModuleUses
        |> List.choose (fun dep ->
            let sourceModule =
                modules
                |> List.tryFind (fun m -> m.fullName = dep.source)

            match sourceModule with
            | Some source ->
                match Map.tryFind (source.projectId, dep.target) moduleByQualifiedName with
                | Some target ->
                    Some {
                        sourceId = source.id
                        targetId = target.id
                        kind = "module-use"
                        details = dep.details
                    }
                | None -> None
            | None -> None)
        |> List.distinctBy (fun e -> e.sourceId, e.targetId, e.kind)

    let rawTypeDependencies =
        rawTypes
        |> List.collect (fun source ->
            let sourceId = makeId "type" (source.projectPath + "|" + source.filePath + "|" + source.fullName)
            let sourceProjectId = projectIds[source.projectPath]

            source.referencedTypeNames
            |> List.choose (fun targetName ->
                let targetExact = Map.tryFind (sourceProjectId, targetName) typesByProjectAndName
                let targetLeaf = Map.tryFind (sourceProjectId, targetName.Split('.') |> Array.last) typesByProjectAndLeafName

                match targetExact, targetLeaf with
                | Some target, _ when target.id <> sourceId -> Some(sourceId, target.id, targetName)
                | None, Some [ target ] when target.id <> sourceId -> Some(sourceId, target.id, targetName)
                | _ -> None))

    let typeUseEdges =
        rawTypeDependencies
        |> List.distinct
        |> List.map (fun (sourceId, targetId, details) ->
            {
                sourceId = sourceId
                targetId = targetId
                kind = "type-use"
                details = details
            })

    let containmentEdges =
        let folderContainment =
            files
            |> List.map (fun f ->
                {
                    sourceId = f.folderId
                    targetId = f.id
                    kind = "contains-file"
                    details = "folder"
                })

        let moduleContainment =
            modules
            |> List.map (fun m ->
                {
                    sourceId = m.fileId
                    targetId = m.id
                    kind = "contains-module"
                    details = "file"
                })

        let typeContainment =
            types
            |> List.map (fun t ->
                let source = defaultArg t.moduleId t.fileId
                {
                    sourceId = source
                    targetId = t.id
                    kind = "contains-type"
                    details = "module-or-file"
                })

        List.concat [ folderContainment; moduleContainment; typeContainment ]

    let allEdges =
        List.concat [
            projectReferenceEdges
            compileOrderEdges
            moduleUseEdges
            typeUseEdges
            containmentEdges
        ]
        |> List.sortBy (fun e -> e.kind, e.sourceId, e.targetId)

    {
        schemaVersion = "0.2.0"
        generatedAtUtc = DateTime.UtcNow.ToString("O")
        rootPath = rootPath
        projects = projects
        folders = folders
        files = files
        modules = modules
        types = types
        edges = allEdges
        warnings = warnings |> Seq.distinct |> Seq.sort |> Seq.toList
    }

let jsonOptions =
    let opts = JsonSerializerOptions(WriteIndented = true)
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let writeJson (outputPath: string) (model: ArchitectureModel) =
    let json = JsonSerializer.Serialize(model, jsonOptions)
    File.WriteAllText(outputPath, json)

let escapeLabel (text: string) =
    text.Replace("\"", "'")

let mermaidHeader (direction: string) =
    [
        "%%{init: {'flowchart': {'defaultRenderer': 'elk'}} }%%"
        $"flowchart {direction}"
    ]

let appendLines (sb: StringBuilder) (lines: string seq) =
    lines |> Seq.iter (fun line -> sb.AppendLine(line) |> ignore)

let generateOverviewMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    appendLines sb (mermaidHeader "LR")

    for p in model.projects do
        sb.AppendLine($"  {p.id}[\"Project: {escapeLabel p.name}\"]") |> ignore

    for m in model.modules do
        sb.AppendLine($"  {m.id}[\"Module: {escapeLabel m.fullName}\"]") |> ignore
        sb.AppendLine($"  {m.projectId} -. contains .-> {m.id}") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "project-reference") do
        sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "module-use") do
        sb.AppendLine($"  {e.sourceId} -->|uses| {e.targetId}") |> ignore

    sb.ToString()

let generateCompileOrderMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    appendLines sb (mermaidHeader "TB")

    let filesById = model.files |> Seq.map (fun f -> f.id, f) |> Map.ofSeq

    for p in model.projects do
        sb.AppendLine($"  subgraph {p.id}[\"{escapeLabel p.name} compile order\"]") |> ignore
        for fileId in p.fileIds do
            match Map.tryFind fileId filesById with
            | Some file -> sb.AppendLine($"    {file.id}[\"{escapeLabel file.path}\"]") |> ignore
            | None -> ()
        sb.AppendLine("  end") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "compile-order") do
        sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    sb.ToString()

let generateFileCompositionMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    appendLines sb (mermaidHeader "TB")

    for p in model.projects do
        sb.AppendLine($"  subgraph comp_{p.id}[\"{escapeLabel p.name}\"]") |> ignore

        let projectFolders = model.folders |> List.filter (fun f -> f.projectId = p.id)
        for folder in projectFolders do
            sb.AppendLine($"    subgraph comp_{folder.id}[\"{escapeLabel folder.path}\"]") |> ignore
            let folderFiles = model.files |> List.filter (fun f -> f.folderId = folder.id)
            for file in folderFiles do
                sb.AppendLine($"      {file.id}[\"File: {escapeLabel file.path}\"]") |> ignore

                let fileModules = model.modules |> List.filter (fun m -> m.fileId = file.id)
                for m in fileModules do
                    sb.AppendLine($"      {m.id}[\"Module: {escapeLabel m.name}\"]") |> ignore
                    sb.AppendLine($"      {file.id} -. contains .-> {m.id}") |> ignore

                let fileTypes = model.types |> List.filter (fun t -> t.fileId = file.id)
                for t in fileTypes do
                    sb.AppendLine($"      {t.id}[\"Type: {escapeLabel t.name} ({escapeLabel t.kind})\"]") |> ignore

                    match t.moduleId with
                    | Some moduleId -> sb.AppendLine($"      {moduleId} -. contains .-> {t.id}") |> ignore
                    | None -> sb.AppendLine($"      {file.id} -. contains .-> {t.id}") |> ignore

            sb.AppendLine("    end") |> ignore

        sb.AppendLine("  end") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "module-use" || e.kind = "type-use") do
        let label = if e.kind = "module-use" then "uses" else "type"
        sb.AppendLine($"  {e.sourceId} -->|{label}| {e.targetId}") |> ignore

    sb.ToString()

let stronglyConnectedComponents (edges: (string * string) list) =
    let adjacency =
        edges
        |> Seq.groupBy fst
        |> Seq.map (fun (source, outgoing) -> source, outgoing |> Seq.map snd |> Seq.distinct |> Seq.toList)
        |> Map.ofSeq

    let nodes =
        edges
        |> Seq.collect (fun (s, t) -> [ s; t ])
        |> Seq.distinct
        |> Seq.toList

    let mutable index = 0
    let stack = ResizeArray<string>()
    let onStack = System.Collections.Generic.HashSet<string>()
    let indices = System.Collections.Generic.Dictionary<string, int>()
    let lowLinks = System.Collections.Generic.Dictionary<string, int>()
    let components = ResizeArray<string list>()

    let rec strongConnect (v: string) =
        indices[v] <- index
        lowLinks[v] <- index
        index <- index + 1
        stack.Add(v)
        onStack.Add(v) |> ignore

        for w in Map.tryFind v adjacency |> Option.defaultValue [] do
            if not (indices.ContainsKey(w)) then
                strongConnect w
                lowLinks[v] <- min lowLinks[v] lowLinks[w]
            elif onStack.Contains(w) then
                lowLinks[v] <- min lowLinks[v] indices[w]

        if lowLinks[v] = indices[v] then
            let mutable continuePop = true
            let scc = ResizeArray<string>()
            while continuePop && stack.Count > 0 do
                let w = stack[stack.Count - 1]
                stack.RemoveAt(stack.Count - 1)
                onStack.Remove(w) |> ignore
                scc.Add(w)
                if w = v then
                    continuePop <- false
            components.Add(scc |> Seq.toList)

    for node in nodes do
        if not (indices.ContainsKey(node)) then
            strongConnect node

    components |> Seq.toList

let generateCyclesMermaid (model: ArchitectureModel) =
    let moduleEdges =
        model.edges
        |> List.filter (fun e -> e.kind = "module-use")
        |> List.map (fun e -> e.sourceId, e.targetId)

    let cyclicSets =
        stronglyConnectedComponents moduleEdges
        |> List.filter (fun scc -> scc.Length > 1)

    let sb = StringBuilder()
    appendLines sb (mermaidHeader "LR")

    if List.isEmpty cyclicSets then
        sb.AppendLine("  no_cycles[\"No module dependency cycles detected\"]") |> ignore
    else
        for cycle in cyclicSets do
            let cycleId = makeId "cycle" (String.concat "|" (cycle |> List.sort))
            sb.AppendLine($"  subgraph {cycleId}[\"Cycle {cycleId}\"]") |> ignore
            for nodeId in cycle do
                sb.AppendLine($"    {nodeId}") |> ignore
            sb.AppendLine("  end") |> ignore

        for e in model.edges |> List.filter (fun e -> e.kind = "module-use") do
            let inCycle = cyclicSets |> List.exists (fun c -> List.contains e.sourceId c && List.contains e.targetId c)
            if inCycle then
                sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    sb.ToString()

let generateCouplingMermaid (model: ArchitectureModel) =
    let moduleEdges =
        model.edges
        |> List.filter (fun e -> e.kind = "module-use")

    let edgeSet =
        moduleEdges
        |> Seq.map (fun e -> e.sourceId, e.targetId)
        |> Set.ofSeq

    let stronglyCoupledPairs =
        edgeSet
        |> Seq.choose (fun (a, b) ->
            if a < b && Set.contains (b, a) edgeSet then Some(a, b) else None)
        |> Seq.toList

    let sb = StringBuilder()
    appendLines sb (mermaidHeader "LR")

    if List.isEmpty stronglyCoupledPairs then
        sb.AppendLine("  no_coupling[\"No unusually strong coupling detected\"]") |> ignore
    else
        for (a, b) in stronglyCoupledPairs do
            sb.AppendLine($"  {a} -->|coupled| {b}") |> ignore
            sb.AppendLine($"  {b} -->|coupled| {a}") |> ignore

    sb.ToString()

let generateNeighborhoodMermaid (model: ArchitectureModel) (focusSymbol: string option) =
    let dependencyEdges =
        model.edges
        |> List.filter (fun e -> e.kind = "module-use" || e.kind = "type-use")

    let degreeByNode =
        dependencyEdges
        |> List.collect (fun e -> [ e.sourceId, 1; e.targetId, 1 ])
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> k, (v |> Seq.sumBy snd))
        |> Map.ofSeq

    let focusId =
        focusSymbol
        |> Option.bind (fun symbol ->
            model.modules
            |> List.tryFind (fun m -> String.Equals(m.fullName, symbol, StringComparison.OrdinalIgnoreCase))
            |> Option.map (fun m -> m.id)
            |> Option.orElseWith (fun () ->
                model.types
                |> List.tryFind (fun t -> String.Equals(t.fullName, symbol, StringComparison.OrdinalIgnoreCase))
                |> Option.map (fun t -> t.id)))
        |> Option.orElseWith (fun () ->
            degreeByNode
            |> Map.toList
            |> List.sortByDescending snd
            |> List.tryHead
            |> Option.map fst)

    let sb = StringBuilder()
    appendLines sb (mermaidHeader "LR")

    match focusId with
    | None -> sb.AppendLine("  empty[\"No dependency neighborhood available\"]") |> ignore
    | Some focus ->
        let neighborhoodNodes =
            dependencyEdges
            |> List.collect (fun e ->
                if e.sourceId = focus || e.targetId = focus then [ e.sourceId; e.targetId ] else [])
            |> Set.ofList
            |> Set.add focus

        for node in neighborhoodNodes do
            sb.AppendLine($"  {node}") |> ignore

        for e in dependencyEdges do
            if Set.contains e.sourceId neighborhoodNodes && Set.contains e.targetId neighborhoodNodes then
                let label = if e.kind = "module-use" then "module" else "type"
                sb.AppendLine($"  {e.sourceId} -->|{label}| {e.targetId}") |> ignore

    sb.ToString()

let generateIndexHtml (model: ArchitectureModel) =
    let warningItems =
        if List.isEmpty model.warnings then "<li>None</li>"
        else model.warnings |> List.map escapeLabel |> List.map (fun w -> $"<li>{w}</li>") |> String.concat "\n"

    $"""
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Architecture Review</title>
  <style>
    body {{ font-family: Segoe UI, sans-serif; margin: 1.5rem; background: #f5f7fb; color: #111827; }}
    .card {{ background: #ffffff; border: 1px solid #d1d5db; border-radius: 0.75rem; padding: 1rem; margin-bottom: 1rem; }}
    h1, h2 {{ margin-top: 0; }}
    ul {{ margin: 0.25rem 0 0.75rem 1.25rem; }}
  </style>
</head>
<body>
  <h1>Architecture Review Output</h1>
  <div class="card">
    <p><strong>Root:</strong> {model.rootPath}</p>
    <p><strong>Projects:</strong> {model.projects.Length}</p>
    <p><strong>Folders:</strong> {model.folders.Length}</p>
    <p><strong>Files:</strong> {model.files.Length}</p>
    <p><strong>Modules:</strong> {model.modules.Length}</p>
    <p><strong>Types:</strong> {model.types.Length}</p>
    <p><strong>Edges:</strong> {model.edges.Length}</p>
    <p><a href="architecture.json">Open architecture.json</a></p>
  </div>
  <div class="card">
    <h2>Focused diagrams</h2>
    <ul>
      <li><a href="overview.mmd">High-level project and module overview</a></li>
      <li><a href="file-composition.mmd">File composition with modules and types</a></li>
      <li><a href="compile-order.mmd">Compile-order view</a></li>
      <li><a href="neighborhood.mmd">Dependencies around selected/high-degree symbol</a></li>
      <li><a href="cycles.mmd">Dependency cycles view</a></li>
      <li><a href="coupling.mmd">Strong coupling view</a></li>
    </ul>
  </div>
  <div class="card">
    <h2>Warnings</h2>
    <ul>
      {warningItems}
    </ul>
  </div>
</body>
</html>
"""

let ensureDirectory path =
    if not (Directory.Exists(path)) then
        Directory.CreateDirectory(path) |> ignore

let parseArgs (argv: string array) =
    let args = argv |> Array.toList

    let rec parseTail opts remaining =
        match remaining with
        | [] -> Ok opts
        | ("--output" | "-o") :: output :: tail -> parseTail { opts with outputFolder = Some output } tail
        | "--focus" :: focus :: tail -> parseTail { opts with focusSymbol = Some focus } tail
        | unknown :: _ -> Error $"Unknown argument: {unknown}"

    match args with
    | targetFolder :: tail when not (targetFolder.StartsWith("-")) ->
        parseTail { targetFolder = targetFolder; outputFolder = None; focusSymbol = None } tail
    | _ -> Error "Missing target folder"

let run (options: CliOptions) =
    let targetFolder = normalizePath options.targetFolder
    let outputFolder =
        options.outputFolder
        |> Option.defaultValue (Path.Combine(targetFolder, ".architecture-review"))
        |> normalizePath

    if not (Directory.Exists(targetFolder)) then
        eprintfn "Target folder does not exist: %s" targetFolder
        1
    else
        ensureDirectory outputFolder

        let model = buildModel targetFolder
        let jsonPath = Path.Combine(outputFolder, "architecture.json")
        let overviewPath = Path.Combine(outputFolder, "overview.mmd")
        let compositionPath = Path.Combine(outputFolder, "file-composition.mmd")
        let compileOrderPath = Path.Combine(outputFolder, "compile-order.mmd")
        let neighborhoodPath = Path.Combine(outputFolder, "neighborhood.mmd")
        let cyclesPath = Path.Combine(outputFolder, "cycles.mmd")
        let couplingPath = Path.Combine(outputFolder, "coupling.mmd")
        let htmlPath = Path.Combine(outputFolder, "index.html")

        writeJson jsonPath model
        File.WriteAllText(overviewPath, generateOverviewMermaid model)
        File.WriteAllText(compositionPath, generateFileCompositionMermaid model)
        File.WriteAllText(compileOrderPath, generateCompileOrderMermaid model)
        File.WriteAllText(neighborhoodPath, generateNeighborhoodMermaid model options.focusSymbol)
        File.WriteAllText(cyclesPath, generateCyclesMermaid model)
        File.WriteAllText(couplingPath, generateCouplingMermaid model)
        File.WriteAllText(htmlPath, generateIndexHtml model)

        printfn "Analyzed root: %s" targetFolder
        printfn "Projects: %d" model.projects.Length
        printfn "Folders: %d" model.folders.Length
        printfn "Files: %d" model.files.Length
        printfn "Modules: %d" model.modules.Length
        printfn "Types: %d" model.types.Length
        printfn "Edges: %d" model.edges.Length
        printfn "Warnings: %d" model.warnings.Length
        printfn "Output written to: %s" outputFolder
        0

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Ok opts -> run opts
    | Error message ->
        eprintfn "%s" message
        eprintfn "Usage: architecture-review <target-folder> [--output <folder>] [--focus <module-or-type-full-name>]"
        1

