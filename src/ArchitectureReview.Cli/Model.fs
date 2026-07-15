module ArchitectureReview.Model

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
}
