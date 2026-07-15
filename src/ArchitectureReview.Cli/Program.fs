open System
open System.IO
open ArchitectureReview.Common
open ArchitectureReview.Model
open ArchitectureReview.Builder
open ArchitectureReview.Rendering

let parseArgs (argv: string array) =
    let args = argv |> Array.toList

    let rec parseTail opts remaining =
        match remaining with
        | [] -> Ok opts
        | ("--output" | "-o") :: output :: tail -> parseTail { opts with outputFolder = Some output } tail
        | unknown :: _ -> Error $"Unknown argument: {unknown}"

    match args with
    | targetFolder :: tail when not (targetFolder.StartsWith("-")) ->
        parseTail { targetFolder = targetFolder; outputFolder = None } tail
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

        let staleArtifacts = [
            "overview.mmd"
            "file-composition.mmd"
            "compile-order.mmd"
            "neighborhood.mmd"
            "cycles.mmd"
            "coupling.mmd"
            "viewer.html"
        ]

        for artifact in staleArtifacts do
            let artifactPath = Path.Combine(outputFolder, artifact)
            if File.Exists(artifactPath) then
                File.Delete(artifactPath)

        let model = buildModel targetFolder
        let jsonPath = Path.Combine(outputFolder, "architecture.json")
        let htmlPath = Path.Combine(outputFolder, "index.html")

        let diagrams = [
            ("High-level project and module overview", generateOverviewMermaid model)
            ("File composition with modules and types", generateFileCompositionMermaid model)
            ("Compile-order view", generateCompileOrderMermaid model)
        ]

        writeJson jsonPath model
        File.WriteAllText(htmlPath, generateIndexHtml diagrams)

        printfn "Analyzed root: %s" targetFolder
        printfn "Projects: %d" model.projects.Length
        printfn "Folders: %d" model.folders.Length
        printfn "Files: %d" model.files.Length
        printfn "Modules: %d" model.modules.Length
        printfn "Types: %d" model.types.Length
        printfn "Edges: %d" model.edges.Length
        printfn "Warnings: %d" model.warnings.Length
        printfn "Output written to: %s" htmlPath
        0

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Ok opts -> run opts
    | Error message ->
        eprintfn "%s" message
        eprintfn "Usage: architecture-review <target-folder> [--output <folder>]"
        1

