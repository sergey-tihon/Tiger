module Chap04_Parser

open System
open System.IO
open FSharp.Text.Lexing
open NUnit.Framework
open FsUnit

let testCases = Config.TestCasesFiles

//[<Test; TestCaseSource("testCases")>]
let parserTest fname =
    printfn "%s" <| File.ReadAllText(fname).TrimEnd()
    printfn "========================================="

    use reader = File.OpenText(fname)
    let lexbuf = LexBuffer<char>.FromTextReader reader
    let ast = Parser.start Lexer.tokenize lexbuf

    printfn "%A" ast
    printfn "========================================="

[<Test>]
let parserTestManual () =
    let text = "for x:=0 to 10 do (y:=42)"

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

    printfn "%A" ast
    printfn "========================================="
