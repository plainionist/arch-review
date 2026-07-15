module ArchitectureReview.Builder

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open ArchitectureReview.Common
open ArchitectureReview.Model
open ArchitectureReview.Parsing

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
    let rawModuleTypeUses = extraction |> List.collect (fun e -> e.moduleTypeUses)

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

    let directlyReferencedProjectIdsByProjectId =
        parsedProjects
        |> List.map (fun p ->
            let sourceProjectId = projectIds[p.projectPath]
            let referencedIds =
                p.projectReferences
                |> List.choose (fun projectRef ->
                    if Set.contains projectRef knownProjects then Some projectIds[projectRef]
                    else None)
                |> Set.ofList

            sourceProjectId, referencedIds)
        |> Map.ofList

    let referencedProjectIdsByProjectId =
        let rec collectTransitively (visited: Set<string>) (pending: string list) =
            match pending with
            | [] -> visited
            | current :: rest when Set.contains current visited -> collectTransitively visited rest
            | current :: rest ->
                let direct = directlyReferencedProjectIdsByProjectId |> Map.tryFind current |> Option.defaultValue Set.empty
                collectTransitively (Set.add current visited) (rest @ (direct |> Set.toList))

        projectIds
        |> Map.toSeq
        |> Seq.map (fun (_, pid) ->
            let direct = directlyReferencedProjectIdsByProjectId |> Map.tryFind pid |> Option.defaultValue Set.empty
            let closure = collectTransitively Set.empty (direct |> Set.toList)
            pid, closure)
        |> Map.ofSeq

    let moduleUseEdges =
        let candidatePrefixes (name: string) =
            let segments = name.Split('.', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            [ for len in segments.Length .. -1 .. 1 -> segments |> List.take len |> String.concat "." ]

        let tryResolveTargetModule projectId (sourceModuleName: string) (targetName: string) =
            let sourceSegments = sourceModuleName.Split('.', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            let candidateProjectIds =
                projectId
                :: (referencedProjectIdsByProjectId
                    |> Map.tryFind projectId
                    |> Option.defaultValue Set.empty
                    |> Set.toList)

            let scopedCandidates =
                [ for len in sourceSegments.Length .. -1 .. 1 ->
                    let scope = sourceSegments |> List.take len |> String.concat "."
                    if String.IsNullOrWhiteSpace(scope) then targetName else $"{scope}.{targetName}" ]

            let allCandidates = targetName :: scopedCandidates

            allCandidates
            |> List.collect candidatePrefixes
            |> List.distinct
            |> List.tryPick (fun candidate ->
                candidateProjectIds
                |> List.tryPick (fun pid -> Map.tryFind (pid, candidate) moduleByQualifiedName))

        rawModuleUses
        |> List.choose (fun dep ->
            let sourceModule =
                modules
                |> List.tryFind (fun m -> m.fullName = dep.source)

            match sourceModule with
            | Some source ->
                match tryResolveTargetModule source.projectId source.fullName dep.target with
                | Some target when target.id <> source.id ->
                    Some {
                        sourceId = source.id
                        targetId = target.id
                        kind = "module-use"
                        details = dep.details
                    }
                | None -> None
                | _ -> None
            | None -> None)
        |> List.distinctBy (fun e -> e.sourceId, e.targetId, e.kind)

    let candidateProjectIdsFor projectId =
        projectId
        :: (referencedProjectIdsByProjectId
            |> Map.tryFind projectId
            |> Option.defaultValue Set.empty
            |> Set.toList)

    let tryResolveTypeByName sourceProjectId sourceTypeId (targetName: string) =
        let candidateProjectIds = candidateProjectIdsFor sourceProjectId
        let targetLeafName = targetName.Split('.') |> Array.last

        let exactMatches =
            candidateProjectIds
            |> List.choose (fun pid -> Map.tryFind (pid, targetName) typesByProjectAndName)

        match exactMatches with
        | target :: _ when target.id <> sourceTypeId -> Some(target.id, targetName)
        | _ ->
            let leafMatches =
                candidateProjectIds
                |> List.collect (fun pid -> Map.tryFind (pid, targetLeafName) typesByProjectAndLeafName |> Option.defaultValue [])
                |> List.distinctBy (fun t -> t.id)

            match leafMatches with
            | [ target ] when target.id <> sourceTypeId -> Some(target.id, targetName)
            | _ -> None

    let rawTypeDependencies =
        rawTypes
        |> List.collect (fun source ->
            let sourceId = makeId "type" (source.projectPath + "|" + source.filePath + "|" + source.fullName)
            let sourceProjectId = projectIds[source.projectPath]

            source.referencedTypeNames
            |> List.choose (fun targetName ->
                match tryResolveTypeByName sourceProjectId sourceId targetName with
                | Some(targetId, details) -> Some(sourceId, targetId, details)
                | None -> None))

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

    let moduleTypeUseEdges =
        rawModuleTypeUses
        |> List.choose (fun dep ->
            let sourceModule =
                modules
                |> List.tryFind (fun m -> m.fullName = dep.source)

            match sourceModule with
            | Some source ->
                match tryResolveTypeByName source.projectId "" dep.target with
                | Some(targetId, details) ->
                    Some {
                        sourceId = source.id
                        targetId = targetId
                        kind = "module-type-use"
                        details = details
                    }
                | _ -> None
            | None -> None)
        |> List.distinctBy (fun e -> e.sourceId, e.targetId, e.kind)

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
            moduleTypeUseEdges
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
