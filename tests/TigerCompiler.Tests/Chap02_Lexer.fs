module Chap02_Lexer

open System
open System.IO
open FSharp.Text.Lexing
open NUnit.Framework
open FsUnit

let testCases = Config.TestCasesFiles

[<Test; TestCaseSource("testCases")>]
let lexerTest fname =
    printfn "%s" <| File.ReadAllText(fname).TrimEnd()
    printfn "========================================="

    use reader = File.OpenText(fname)
    let buffer = LexBuffer<char>.FromTextReader reader
    let rec loop tokens =
        match Lexer.tokenize buffer with
        | Parser.EOF -> List.rev tokens
        | x -> loop (x::tokens)
    let tokens = loop []

    printfn "%A" tokens
    printfn "========================================="

    tokens |> should not' (be Empty)
