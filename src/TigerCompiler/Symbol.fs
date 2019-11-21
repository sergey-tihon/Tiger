module Tiger.Symbol

open System.Collections.Generic

type Symbol = string * int

let mutable private nextSym = 0
let private hashtable = Dictionary<string, int>(128)

let symbol name =
    match hashtable.TryGetValue name with
    | true, i -> (name, i)
    | _ -> 
        let i = nextSym
        nextSym <- i + 1
        hashtable.Add(name, i)
        (name, i)
