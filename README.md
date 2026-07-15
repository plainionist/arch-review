# architecture-review

## Purpose

`architecture-review` is a small CLI tool that analyzes an F# codebase and produces a machine-readable architecture model plus focused Mermaid diagrams.

## Usage

Run with default output folder (`<target-folder>/.architecture-review`):

```powershell
dotnet run --project src/ArchitectureReview.Cli/ArchitectureReview.Cli.fsproj -- <target-folder>
```

Run with explicit output folder:

```powershell
dotnet run --project src/ArchitectureReview.Cli/ArchitectureReview.Cli.fsproj -- <target-folder> --output <output-folder>
```

Run with focused neighborhood symbol:

```powershell
dotnet run --project src/ArchitectureReview.Cli/ArchitectureReview.Cli.fsproj -- <target-folder> --focus <module-or-type-full-name>
```
