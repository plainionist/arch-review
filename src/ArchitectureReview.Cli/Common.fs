module ArchitectureReview.Common

open System
open System.IO
open System.Security.Cryptography
open System.Text

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

let escapeLabel (text: string) =
    text.Replace("\"", "'")

let escapeHtml (text: string) =
    text.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")

let ensureDirectory path =
    if not (Directory.Exists(path)) then
        Directory.CreateDirectory(path) |> ignore
