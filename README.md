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
