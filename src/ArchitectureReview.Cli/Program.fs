open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Xml.Linq

type ProjectNode = {
    id: string
    name: string
    path: string
    folder: string
    fileIds: string list
}

type FileNode = {
    id: string
    projectId: string
    path: string
    compileOrder: int
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
    files: FileNode list
    edges: Edge list
    warnings: string list
}

type ParsedProject = {
    projectPath: string
    projectName: string
    compileFiles: string list
    projectReferences: string list
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

let buildModel (rootPath: string) =
    let rootPath = normalizePath rootPath
    let discoveredProjects = discoverProjects rootPath
    let parsedProjects = discoveredProjects |> List.map parseFsproj

    let projectIds =
        parsedProjects
        |> List.map (fun p -> p.projectPath, makeId "project" p.projectPath)
        |> Map.ofList

    let knownProjects = projectIds |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    let warnings = ResizeArray<string>()

    let projects =
        parsedProjects
        |> List.map (fun p ->
            let pid = projectIds[p.projectPath]
            let fileIds = p.compileFiles |> List.map (makeId "file")
            {
                id = pid
                name = p.projectName
                path = relPath rootPath p.projectPath
                folder = relPath rootPath (Path.GetDirectoryName(p.projectPath))
                fileIds = fileIds
            })

    let files =
        parsedProjects
        |> List.collect (fun p ->
            let pid = projectIds[p.projectPath]
            p.compileFiles
            |> List.mapi (fun idx filePath ->
                {
                    id = makeId "file" filePath
                    projectId = pid
                    path = relPath rootPath filePath
                    compileOrder = idx
                }))

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

    warnings.Add("Module/type dependency extraction is not implemented yet; current output includes project and compile-order edges only.")

    {
        schemaVersion = "0.1.0"
        generatedAtUtc = DateTime.UtcNow.ToString("O")
        rootPath = rootPath
        projects = projects
        files = files
        edges = List.append projectReferenceEdges compileOrderEdges
        warnings = warnings |> Seq.toList
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

let generateOverviewMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    sb.AppendLine("%%{init: {'flowchart': {'defaultRenderer': 'elk'}} }%%") |> ignore
    sb.AppendLine("flowchart LR") |> ignore

    for p in model.projects do
        sb.AppendLine($"  {p.id}[\"{escapeLabel p.name}\"]") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "project-reference") do
        sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    sb.ToString()

let generateCompileOrderMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    sb.AppendLine("%%{init: {'flowchart': {'defaultRenderer': 'elk'}} }%%") |> ignore
    sb.AppendLine("flowchart TB") |> ignore

    let filesById = model.files |> Seq.map (fun f -> f.id, f) |> Map.ofSeq

    for p in model.projects do
        sb.AppendLine($"  subgraph {p.id}[\"{escapeLabel p.name} compile order\"]") |> ignore
        for fileId in p.fileIds do
            match Map.tryFind fileId filesById with
            | Some file ->
                sb.AppendLine($"    {file.id}[\"{escapeLabel file.path}\"]") |> ignore
            | None -> ()
        sb.AppendLine("  end") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "compile-order") do
        sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    sb.ToString()

let generateIndexHtml (model: ArchitectureModel) =
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
    pre {{ white-space: pre-wrap; }}
  </style>
</head>
<body>
  <h1>Architecture Review Output</h1>
  <div class="card">
    <p><strong>Root:</strong> {model.rootPath}</p>
    <p><strong>Projects:</strong> {model.projects.Length}</p>
    <p><strong>Files:</strong> {model.files.Length}</p>
    <p><strong>Edges:</strong> {model.edges.Length}</p>
    <p><a href="architecture.json">Open architecture.json</a></p>
  </div>
  <div class="card">
    <h2>Focused diagrams</h2>
    <ul>
      <li><a href="overview.mmd">Project overview Mermaid source</a></li>
      <li><a href="compile-order.mmd">Compile-order Mermaid source</a></li>
    </ul>
    <p>Copy Mermaid files into your preferred renderer or Mermaid-enabled Markdown viewer.</p>
  </div>
</body>
</html>
"""

let ensureDirectory path =
    if not (Directory.Exists(path)) then
        Directory.CreateDirectory(path) |> ignore

let run (targetFolder: string) (outputFolder: string) =
    let targetFolder = normalizePath targetFolder
    let outputFolder = normalizePath outputFolder

    if not (Directory.Exists(targetFolder)) then
        eprintfn "Target folder does not exist: %s" targetFolder
        1
    else
        ensureDirectory outputFolder

        let model = buildModel targetFolder
        let jsonPath = Path.Combine(outputFolder, "architecture.json")
        let overviewPath = Path.Combine(outputFolder, "overview.mmd")
        let compileOrderPath = Path.Combine(outputFolder, "compile-order.mmd")
        let htmlPath = Path.Combine(outputFolder, "index.html")

        writeJson jsonPath model
        File.WriteAllText(overviewPath, generateOverviewMermaid model)
        File.WriteAllText(compileOrderPath, generateCompileOrderMermaid model)
        File.WriteAllText(htmlPath, generateIndexHtml model)

        printfn "Analyzed root: %s" targetFolder
        printfn "Projects discovered: %d" model.projects.Length
        printfn "Files discovered: %d" model.files.Length
        printfn "Edges discovered: %d" model.edges.Length
        printfn "Output written to: %s" outputFolder
        0

[<EntryPoint>]
let main argv =
    match argv |> Array.toList with
    | [ targetFolder ] ->
        let defaultOutput = Path.Combine(normalizePath targetFolder, ".architecture-review")
        run targetFolder defaultOutput
    | [ targetFolder; "--output"; outputFolder ]
    | [ targetFolder; "-o"; outputFolder ] ->
        run targetFolder outputFolder
    | _ ->
        eprintfn "Usage: architecture-review <target-folder> [--output <folder>]"
        1

