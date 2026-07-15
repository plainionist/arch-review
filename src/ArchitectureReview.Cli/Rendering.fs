module ArchitectureReview.Rendering

open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open ArchitectureReview.Common
open ArchitectureReview.Model

let jsonOptions =
    let opts = JsonSerializerOptions(WriteIndented = true)
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let writeJson (outputPath: string) (model: ArchitectureModel) =
    let json = JsonSerializer.Serialize(model, jsonOptions)
    File.WriteAllText(outputPath, json)

let mermaidHeader (direction: string) =
    [
        "%%{init: {'flowchart': {'defaultRenderer': 'elk'}} }%%"
        $"flowchart {direction}"
    ]

let appendLines (sb: StringBuilder) (lines: string seq) =
    lines |> Seq.iter (fun line -> sb.AppendLine(line) |> ignore)

let appendNodeClassDefinitions (sb: StringBuilder) =
    sb.AppendLine("  classDef project fill:#0f766e,color:#ffffff,stroke:#115e59,stroke-width:1px") |> ignore
    sb.AppendLine("  classDef folder fill:#0ea5e9,color:#082f49,stroke:#0369a1,stroke-width:1px") |> ignore
    sb.AppendLine("  classDef file fill:#fde68a,color:#3f2f00,stroke:#f59e0b,stroke-width:1px") |> ignore
    sb.AppendLine("  classDef module fill:#a7f3d0,color:#064e3b,stroke:#10b981,stroke-width:1px") |> ignore
    sb.AppendLine("  classDef type fill:#e9d5ff,color:#3b0764,stroke:#a855f7,stroke-width:1px") |> ignore

let generateOverviewMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    appendLines sb (mermaidHeader "LR")
    appendNodeClassDefinitions sb

    for p in model.projects do
        sb.AppendLine($"  {p.id}[\"{escapeLabel p.name}\"]") |> ignore
        sb.AppendLine($"  class {p.id} project") |> ignore

    for m in model.modules do
        sb.AppendLine($"  {m.id}[\"{escapeLabel m.fullName}\"]") |> ignore
        sb.AppendLine($"  {m.projectId} -. contains .-> {m.id}") |> ignore
        sb.AppendLine($"  class {m.id} module") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "project-reference") do
        sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "module-use") do
        sb.AppendLine($"  {e.sourceId} -->|uses| {e.targetId}") |> ignore

    sb.ToString()

let generateCompileOrderMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    appendLines sb (mermaidHeader "TB")
    appendNodeClassDefinitions sb

    let filesById = model.files |> Seq.map (fun f -> f.id, f) |> Map.ofSeq

    for p in model.projects do
        sb.AppendLine($"  subgraph {p.id}[\"{escapeLabel p.name} compile order\"]") |> ignore
        sb.AppendLine($"  class {p.id} project") |> ignore
        for fileId in p.fileIds do
            match Map.tryFind fileId filesById with
            | Some file ->
                sb.AppendLine($"    {file.id}[\"{escapeLabel file.path}\"]") |> ignore
                sb.AppendLine($"    class {file.id} file") |> ignore
            | None -> ()
        sb.AppendLine("  end") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "compile-order") do
        sb.AppendLine($"  {e.sourceId} --> {e.targetId}") |> ignore

    sb.ToString()

let generateFileCompositionMermaid (model: ArchitectureModel) =
    let sb = StringBuilder()
    appendLines sb (mermaidHeader "TB")
    appendNodeClassDefinitions sb

    for p in model.projects do
        sb.AppendLine($"  subgraph comp_{p.id}[\"{escapeLabel p.name}\"]") |> ignore
        sb.AppendLine($"  class {p.id} project") |> ignore

        let projectFolders = model.folders |> List.filter (fun f -> f.projectId = p.id)
        for folder in projectFolders do
            sb.AppendLine($"    subgraph comp_{folder.id}[\"{escapeLabel folder.path}\"]") |> ignore
            sb.AppendLine($"    class {folder.id} folder") |> ignore
            let folderFiles = model.files |> List.filter (fun f -> f.folderId = folder.id)
            for file in folderFiles do
                sb.AppendLine($"      {file.id}[\"{escapeLabel file.path}\"]") |> ignore
                sb.AppendLine($"      class {file.id} file") |> ignore

                let fileModules = model.modules |> List.filter (fun m -> m.fileId = file.id)
                for m in fileModules do
                    sb.AppendLine($"      {m.id}[\"{escapeLabel m.name}\"]") |> ignore
                    sb.AppendLine($"      {file.id} -. contains .-> {m.id}") |> ignore
                    sb.AppendLine($"      class {m.id} module") |> ignore

                let fileTypes = model.types |> List.filter (fun t -> t.fileId = file.id)
                for t in fileTypes do
                    sb.AppendLine($"      {t.id}[\"{escapeLabel t.name}\"]") |> ignore
                    sb.AppendLine($"      class {t.id} type") |> ignore

                    match t.moduleId with
                    | Some moduleId -> sb.AppendLine($"      {moduleId} -. contains .-> {t.id}") |> ignore
                    | None -> sb.AppendLine($"      {file.id} -. contains .-> {t.id}") |> ignore

            sb.AppendLine("    end") |> ignore

        sb.AppendLine("  end") |> ignore

    for e in model.edges |> List.filter (fun e -> e.kind = "module-use" || e.kind = "type-use") do
        let label = if e.kind = "module-use" then "uses" else "type"
        sb.AppendLine($"  {e.sourceId} -->|{label}| {e.targetId}") |> ignore

    sb.ToString()

let generateIndexHtml (diagrams: (string * string) list) =
    let sb = StringBuilder()
    sb.AppendLine("<!doctype html>") |> ignore
    sb.AppendLine("<html lang=\"en\">") |> ignore
    sb.AppendLine("<head>") |> ignore
    sb.AppendLine("  <meta charset=\"utf-8\" />") |> ignore
    sb.AppendLine("  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />") |> ignore
    sb.AppendLine("  <title>Architecture Review Charts</title>") |> ignore
    sb.AppendLine("  <style>") |> ignore
    sb.AppendLine("    body { font-family: Segoe UI, sans-serif; margin: 1.5rem; background: #f5f7fb; color: #111827; }") |> ignore
    sb.AppendLine("    .chart { background: #ffffff; border: 1px solid #d1d5db; border-radius: 0.75rem; padding: 1rem; margin-bottom: 1rem; }") |> ignore
    sb.AppendLine("    .legend { display: flex; flex-wrap: wrap; gap: 0.75rem; margin: 0 0 1rem 0; }") |> ignore
    sb.AppendLine("    .legend-item { display: inline-flex; align-items: center; gap: 0.4rem; background: #ffffff; border: 1px solid #d1d5db; border-radius: 999px; padding: 0.3rem 0.7rem; }") |> ignore
    sb.AppendLine("    .swatch { width: 0.9rem; height: 0.9rem; border-radius: 0.2rem; border: 1px solid rgba(0,0,0,0.15); }") |> ignore
    sb.AppendLine("    .project { background: #0f766e; }") |> ignore
    sb.AppendLine("    .folder { background: #0ea5e9; }") |> ignore
    sb.AppendLine("    .file { background: #fde68a; }") |> ignore
    sb.AppendLine("    .module { background: #a7f3d0; }") |> ignore
    sb.AppendLine("    .type { background: #e9d5ff; }") |> ignore
    sb.AppendLine("    h1, h2 { margin-top: 0; }") |> ignore
    sb.AppendLine("    pre.mermaid { background: #ffffff; overflow: auto; }") |> ignore
    sb.AppendLine("  </style>") |> ignore
    sb.AppendLine("  <script type=\"module\">") |> ignore
    sb.AppendLine("    import mermaid from \"https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs\";") |> ignore
    sb.AppendLine("    mermaid.initialize({ startOnLoad: true });") |> ignore
    sb.AppendLine("  </script>") |> ignore
    sb.AppendLine("</head>") |> ignore
    sb.AppendLine("<body>") |> ignore
    sb.AppendLine("  <h1>Architecture Review Charts</h1>") |> ignore
    sb.AppendLine("  <div class=\"legend\">") |> ignore
    sb.AppendLine("    <span class=\"legend-item\"><span class=\"swatch project\"></span>Project</span>") |> ignore
    sb.AppendLine("    <span class=\"legend-item\"><span class=\"swatch folder\"></span>Folder</span>") |> ignore
    sb.AppendLine("    <span class=\"legend-item\"><span class=\"swatch file\"></span>File</span>") |> ignore
    sb.AppendLine("    <span class=\"legend-item\"><span class=\"swatch module\"></span>Module</span>") |> ignore
    sb.AppendLine("    <span class=\"legend-item\"><span class=\"swatch type\"></span>Type</span>") |> ignore
    sb.AppendLine("  </div>") |> ignore

    for (title, mermaidText) in diagrams do
        sb.AppendLine("  <section class=\"chart\">") |> ignore
        sb.AppendLine($"    <h2>{escapeHtml title}</h2>") |> ignore
        sb.AppendLine($"    <pre class=\"mermaid\">{escapeHtml mermaidText}</pre>") |> ignore
        sb.AppendLine("  </section>") |> ignore

    sb.AppendLine("</body>") |> ignore
    sb.AppendLine("</html>") |> ignore
    sb.ToString()
