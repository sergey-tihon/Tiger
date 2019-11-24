module Tiger.Symbol

open System.Collections.Generic

type Symbol = string * int

let mutable private nextSym = 0
let private hashtable = Dictionary<string, int>(128)

let symbol name :Symbol=
    match hashtable.TryGetValue name with
    | true, i -> (name, i)
    | _ -> 
        let i = nextSym
        nextSym <- i + 1
        hashtable.Add(name, i)
        (name, i)
let name : Symbol -> string = fst

type Table<'a> = Map<int,'a>

let empty:Table<'a> = Map.empty
let enter (table:Table<'a>, key:Symbol, value) :Table<'a> =
    Map.add (snd key) value table
let look (table:Table<'a>, key:Symbol) =
    Map.tryFind (snd key) table