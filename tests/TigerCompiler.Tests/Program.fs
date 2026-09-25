module Program

open Expecto

[<EntryPoint>]
let main argv =
    // Tiger.Symbol keeps a global mutable symbol table, so tests must not run in parallel.
    runTestsInAssemblyWithCLIArgs [ Sequenced ] argv
