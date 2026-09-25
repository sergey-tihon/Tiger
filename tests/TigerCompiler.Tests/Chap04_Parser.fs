module Chap04_Parser

open System.IO
open FSharp.Text.Lexing
open Expecto
open Swensen.Unquote

// test49.tig: syntax error, nil should not be preceded by type-id
let private invalidSyntax = set [ "test49.tig" ]

let private parseFile (fname: string) =
    use reader = File.OpenText(fname)
    let lexbuf = LexBuffer<char>.FromTextReader reader
    Parser.start Lexer.tokenize lexbuf

let private manualProgram =
    """
let
  type list = {first: int, rest: list}
  function readlist() : list =
    list{first=0,rest=readlist()}
in ()
end"""

[<Tests>]
let tests =
    testList
        "Parser"
        [
            for fname in Config.TestCasesFiles do
                let name = Path.GetFileName fname

                if not (invalidSyntax.Contains name) then
                    testCase name (fun () -> parseFile fname |> ignore)

            testCase "manual program lexes, parses and type-checks" (fun () ->
                let rec loop lexbuf tokens =
                    match Lexer.tokenize lexbuf with
                    | Parser.EOF as x -> List.rev (x :: tokens)
                    | x -> loop lexbuf (x :: tokens)

                let tokens = loop (LexBuffer<char>.FromString manualProgram) []
                test <@ List.last tokens = Parser.EOF @>

                let ast = Parser.start Lexer.tokenize (LexBuffer<char>.FromString manualProgram)
                Tiger.Semant.transProg ast)
        ]
