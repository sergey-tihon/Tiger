module Config

open System.IO

// Folder with Tiger samples
let TestCasesRoot =
    __SOURCE_DIRECTORY__ + "/../testcases"
    |> Path.GetFullPath

let TestCasesFiles =
    TestCasesRoot
    |> Directory.GetFiles
    |> Array.sort