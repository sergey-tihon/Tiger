module Chap02_Lexer

open System
open System.IO
open FSharp.Text.Lexing
open Expecto

[<Tests>]
let lexerTests =
    Config.TestCasesRoot
    |> Directory.GetFiles
    |> Array.toList
    |> List.map (fun fname ->
        testCase (sprintf "Tokenize(%s)" fname) <| fun _ ->
            use reader = File.OpenText(fname)
            let buffer = LexBuffer<char>.FromTextReader reader
            let rec loop tokens =
                match Lexer.tokenize buffer with
                | Parser.EOF -> tokens
                | x -> loop (x::tokens)
            Expect.equal (loop [] |> List.isEmpty) false
                         "Tokens list is not empty"
       )
    |> testList "Ch02/Lexer"