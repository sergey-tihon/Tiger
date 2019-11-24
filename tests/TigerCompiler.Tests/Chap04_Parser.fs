module Chap04_Parser

open System
open System.IO
open FSharp.Text.Lexing
open NUnit.Framework
open FsUnit

let testCases =
    Config.TestCasesFiles
    |> Array.filter (fun path ->
        let fileName = Path.GetFileName(path)
        fileName <> "test49.tig" // error: syntax error, nil should not be preceded by type-id
       )

[<Test; TestCaseSource("testCases")>]
let parserTest fname =
    printfn "%s" <| File.ReadAllText(fname).TrimEnd()
    printfn "========================================="

    use reader = File.OpenText(fname)
    let lexbuf = LexBuffer<char>.FromTextReader reader
    let ast = Parser.start Lexer.tokenize lexbuf
    //Tiger.Semant.transProg ast

    printfn "%A" ast
    printfn "========================================="

[<Test>]
let parserTestManual () =
    let text = "
let
  type list = {first: int, rest: list}
  function readlist() : list =
    list{first=0,rest=readlist()}
in ()
end"

    printfn "%s" text
    printfn "========================================="

    let lexbuf = LexBuffer<char>.FromString text
    let rec loop tokens =
        match Lexer.tokenize lexbuf with
        | Parser.EOF as x -> List.rev <| x::tokens
        | x -> loop (x::tokens)
    let tokens = loop []
    printfn "%A" tokens
    printfn "========================================="


    let lexbuf = LexBuffer<char>.FromString text
    let ast = Parser.start Lexer.tokenize lexbuf
    Tiger.Semant.transProg ast

    printfn "%A" ast
    printfn "========================================="
