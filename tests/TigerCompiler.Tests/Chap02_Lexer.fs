module Chap02_Lexer

open System.IO
open FSharp.Text.Lexing
open Expecto
open Swensen.Unquote

let tokenize (fname: string) =
    use reader = File.OpenText(fname)
    let buffer = LexBuffer<char>.FromTextReader reader

    let rec loop tokens =
        match Lexer.tokenize buffer with
        | Parser.EOF -> List.rev tokens
        | x -> loop (x :: tokens)

    loop []

[<Tests>]
let tests =
    testList
        "Lexer"
        [
            for fname in Config.TestCasesFiles ->
                testCase (Path.GetFileName fname) (fun () ->
                    let tokens = tokenize fname
                    test <@ not (List.isEmpty tokens) @>)
        ]
