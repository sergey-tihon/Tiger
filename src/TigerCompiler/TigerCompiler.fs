module TigerCompiler

open System.IO
open FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
    printfn "argv=%A" argv
    printfn "========================================="
    printfn "   SOURCE CODE"
    printfn "========================================="
    let fname = argv.[0]
    printfn "%s" <| File.ReadAllText(fname).TrimEnd()
    printfn "========================================="
    printfn "   AST"
    printfn "========================================="
    use reader = File.OpenText(fname)
    let lexbuf = LexBuffer<char>.FromTextReader reader
    let ast = Parser.start Lexer.tokenize lexbuf
    printfn "%A" ast
    printfn "========================================="
    Tiger.Semant.transProg ast
    printfn "========================================="
    0 // return an integer exit code
