module ArchitectureReview.Rendering

open System
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

    let modulesByFile =
        model.modules
        |> List.groupBy (fun m -> m.fileId)
        |> Map.ofList

    let typesByFile =
        model.types
        |> List.groupBy (fun t -> t.fileId)
        |> Map.ofList

    let normalizeFolderRel (folderRel: string) =
        if String.IsNullOrWhiteSpace(folderRel) || folderRel = "." then ""
        else folderRel.Replace('\\', '/').Trim('/')

    let toProjectRelativeFolder (projectFolder: string) (filePath: string) =
        let fileFolder = Path.GetDirectoryName(filePath).Replace('\\', '/')
        if fileFolder = projectFolder then ""
        elif fileFolder.StartsWith(projectFolder + "/", StringComparison.Ordinal) then
            fileFolder.Substring(projectFolder.Length + 1)
        else
            fileFolder
        |> normalizeFolderRel

    let immediateChildFolders (folderPaths: string list) (parentRel: string) =
        let getRemainder (child: string) =
            if String.IsNullOrEmpty(parentRel) then child
            elif child.StartsWith(parentRel + "/", StringComparison.Ordinal) then child.Substring(parentRel.Length + 1)
            else ""

        folderPaths
        |> List.filter (fun child ->
            if String.IsNullOrEmpty(parentRel) then
                not (String.IsNullOrEmpty(child))
            else
                child.StartsWith(parentRel + "/", StringComparison.Ordinal))
        |> List.filter (fun child ->
            let remainder = getRemainder child
            not (String.IsNullOrEmpty(remainder)) && not (remainder.Contains('/')))
        |> List.sort

    let rec renderFolderTree (projectId: string) (folderPaths: string list) (filesWithFolder: (FileNode * string) list) (parentRel: string) (indent: string) =
        let filesInCurrentFolder =
            filesWithFolder
            |> List.filter (fun (_, folderRel) -> folderRel = parentRel)
            |> List.sortBy (fun (f, _) -> f.path)

        for (file, _) in filesInCurrentFolder do
            let fileBoxId = makeId "filebox" (projectId + "|" + file.id)
            sb.AppendLine($"{indent}subgraph {fileBoxId}[\"{escapeLabel (Path.GetFileName(file.path))}\"]") |> ignore
            sb.AppendLine($"{indent}  style {fileBoxId} fill:#fff7ed,stroke:#f59e0b,stroke-width:1px") |> ignore

            let fileModules = Map.tryFind file.id modulesByFile |> Option.defaultValue [] |> List.sortBy (fun m -> m.fullName)
            let fileTypes = Map.tryFind file.id typesByFile |> Option.defaultValue [] |> List.sortBy (fun t -> t.fullName)

            sb.AppendLine($"{indent}  {file.id}[\"{escapeLabel (Path.GetFileName(file.path))}\"]") |> ignore
            sb.AppendLine($"{indent}  class {file.id} file") |> ignore

            for m in fileModules do
                sb.AppendLine($"{indent}  {m.id}[\"{escapeLabel m.name}\"]") |> ignore
                sb.AppendLine($"{indent}  class {m.id} module") |> ignore

            for t in fileTypes do
                sb.AppendLine($"{indent}  {t.id}[\"{escapeLabel t.name}\"]") |> ignore
                sb.AppendLine($"{indent}  class {t.id} type") |> ignore

            sb.AppendLine($"{indent}end") |> ignore

        let childFolders = immediateChildFolders folderPaths parentRel
        for childRel in childFolders do
            let folderBoxId = makeId "folderbox" (projectId + "|" + childRel)
            let folderLabel = childRel.Split('/', StringSplitOptions.RemoveEmptyEntries) |> Array.last
            sb.AppendLine($"{indent}subgraph {folderBoxId}[\"{escapeLabel folderLabel}\"]") |> ignore
            sb.AppendLine($"{indent}  style {folderBoxId} fill:#f0f9ff,stroke:#0369a1,stroke-width:1px") |> ignore
            renderFolderTree projectId folderPaths filesWithFolder childRel (indent + "  ")
            sb.AppendLine($"{indent}end") |> ignore

    for p in model.projects do
        let projectFiles =
            model.files
            |> List.filter (fun f -> f.projectId = p.id)

        let filesWithFolder =
            projectFiles
            |> List.map (fun f -> f, toProjectRelativeFolder p.folder f.path)

        let folderPaths =
            filesWithFolder
            |> List.map snd
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> List.distinct

        let projectBoxId = makeId "projectbox" p.id
        sb.AppendLine($"  subgraph {projectBoxId}[\"{escapeLabel p.name}\"]") |> ignore
        sb.AppendLine($"    style {projectBoxId} fill:#ecfeff,stroke:#115e59,stroke-width:1px") |> ignore
        renderFolderTree p.id folderPaths filesWithFolder "" "    "
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
    sb.AppendLine("") |> ignore
    sb.AppendLine("    const clamp = (value, min, max) => Math.min(max, Math.max(min, value));") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("    function enablePanZoomForDiagram(container) {") |> ignore
    sb.AppendLine("      const svg = container.querySelector('svg');") |> ignore
    sb.AppendLine("      if (!svg) return;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      let scale = 1;") |> ignore
    sb.AppendLine("      let translateX = 0;") |> ignore
    sb.AppendLine("      let translateY = 0;") |> ignore
    sb.AppendLine("      let dragging = false;") |> ignore
    sb.AppendLine("      let lastX = 0;") |> ignore
    sb.AppendLine("      let lastY = 0;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      container.style.overflow = 'auto';") |> ignore
    sb.AppendLine("      svg.style.transformOrigin = '0 0';") |> ignore
    sb.AppendLine("      svg.style.willChange = 'transform';") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      const applyTransform = () => {") |> ignore
    sb.AppendLine("        svg.style.transform = `translate(${translateX}px, ${translateY}px) scale(${scale})`; ") |> ignore
    sb.AppendLine("      };") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      container.addEventListener('wheel', (event) => {") |> ignore
    sb.AppendLine("        if (!event.ctrlKey) return;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        event.preventDefault();") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        const zoomFactor = event.deltaY < 0 ? 1.1 : 0.9;") |> ignore
    sb.AppendLine("        const previousScale = scale;") |> ignore
    sb.AppendLine("        scale = clamp(scale * zoomFactor, 0.3, 5);") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        const rect = svg.getBoundingClientRect();") |> ignore
    sb.AppendLine("        const offsetX = event.clientX - rect.left;") |> ignore
    sb.AppendLine("        const offsetY = event.clientY - rect.top;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        const ratio = scale / previousScale;") |> ignore
    sb.AppendLine("        translateX = offsetX - (offsetX - translateX) * ratio;") |> ignore
    sb.AppendLine("        translateY = offsetY - (offsetY - translateY) * ratio;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        applyTransform();") |> ignore
    sb.AppendLine("      }, { passive: false });") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      svg.addEventListener('mousedown', (event) => {") |> ignore
    sb.AppendLine("        if (!event.ctrlKey || event.button !== 0) return;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        dragging = true;") |> ignore
    sb.AppendLine("        lastX = event.clientX;") |> ignore
    sb.AppendLine("        lastY = event.clientY;") |> ignore
    sb.AppendLine("        svg.style.cursor = 'grabbing';") |> ignore
    sb.AppendLine("        event.preventDefault();") |> ignore
    sb.AppendLine("      });") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      window.addEventListener('mousemove', (event) => {") |> ignore
    sb.AppendLine("        if (!dragging) return;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        const dx = event.clientX - lastX;") |> ignore
    sb.AppendLine("        const dy = event.clientY - lastY;") |> ignore
    sb.AppendLine("        lastX = event.clientX;") |> ignore
    sb.AppendLine("        lastY = event.clientY;") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("        translateX += dx;") |> ignore
    sb.AppendLine("        translateY += dy;") |> ignore
    sb.AppendLine("        applyTransform();") |> ignore
    sb.AppendLine("      });") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      window.addEventListener('mouseup', () => {") |> ignore
    sb.AppendLine("        if (!dragging) return;") |> ignore
    sb.AppendLine("        dragging = false;") |> ignore
    sb.AppendLine("        svg.style.cursor = 'default';") |> ignore
    sb.AppendLine("      });") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      applyTransform();") |> ignore
    sb.AppendLine("    }") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("    window.addEventListener('load', () => {") |> ignore
    sb.AppendLine("      const hook = () => {") |> ignore
    sb.AppendLine("        document.querySelectorAll('pre.mermaid').forEach(enablePanZoomForDiagram);") |> ignore
    sb.AppendLine("      };") |> ignore
    sb.AppendLine("") |> ignore
    sb.AppendLine("      setTimeout(hook, 0);") |> ignore
    sb.AppendLine("      setTimeout(hook, 250);") |> ignore
    sb.AppendLine("      setTimeout(hook, 1000);") |> ignore
    sb.AppendLine("    });") |> ignore
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
