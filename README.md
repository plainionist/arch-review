# Build an F# Architecture Review Tool

Create a small command-line tool called `architecture-review`.

The goal is to help developers and AI agents understand and review the architecture of an F# codebase without manually reading all source files.

The tool should analyze an F# solution or project and build a structured architecture model containing:

* projects and folders
* source files
* modules and types contained in each file
* dependencies between modules and types
* F# file compile order

Keep architecture extraction, the architecture model, filtering, and diagram rendering separate.

The tool should produce a machine-readable architecture model, such as JSON, so that AI agents can query and analyze the architecture directly.

From this model, generate focused architecture diagrams using Mermaid.

Use Mermaid flowcharts with the ELK layout engine where appropriate. The generated Mermaid should be structured so that folders, files, modules, and types are visually grouped and dependencies remain readable.

Support different focused views, for example:

* high-level project and module overview
* file composition showing modules and types
* dependencies around a selected module or type
* compile-order view
* dependency cycles and unusually strong coupling

Do not generate one huge diagram by default. Prefer small, focused views derived from the same architecture model.

The first implementation should be simple, deterministic, scriptable, and usable by both humans and AI agents.

For F# analysis, prefer compiler-based information over regex-based source parsing where practical.

## Current implementation (starter)

The repository now contains an initial CLI project at `src/ArchitectureReview.Cli`.

### Run

```powershell
dotnet run --project src/ArchitectureReview.Cli/ArchitectureReview.Cli.fsproj -- <target-folder>
```

Optional output folder:

```powershell
dotnet run --project src/ArchitectureReview.Cli/ArchitectureReview.Cli.fsproj -- <target-folder> --output <output-folder>
```

Default output folder is `<target-folder>/.architecture-review`.

### Generated artifacts

* `architecture.json`
* `overview.mmd`
* `compile-order.mmd`
* `index.html`

### Included in this first cut

* recursive discovery of `.fsproj` files
* project reference edges from `ProjectReference`
* file compile-order edges from `Compile` item order
* deterministic JSON model emission
* focused Mermaid outputs and an HTML index

### Not implemented yet

* compiler-driven extraction of modules and types
* module-to-module and type-to-type dependency edges
* cycle and coupling analysis views
