module Tiger.Types

open System
open Tiger

type Unique = obj ref

[<CustomEquality; NoComparison>]
type Ty =
    | INT
    | STRING
    | RECORD of (Symbol.Symbol * Ty) list * Unique
    | ARRAY of Ty * Unique
    | NIL
    | UNIT
    | NAME of Symbol.Symbol * (Ty option) ref

    override __.GetHashCode() = 0
    override this.Equals(thatObj) =
        let eq (that:Ty) =
            match this, that with
            | INT, INT -> true
            | STRING, STRING -> true
            | RECORD(_, x), RECORD(_,  y) -> x = y
            | ARRAY(_,x), ARRAY(_,y) -> x=y
            | NIL, NIL -> true
            | UNIT, UNIT -> true
            | NAME(s1,t1), NAME(s2,t2) -> s1=s2 && !t1 = !t2
            | _ -> false
        match thatObj with
        | :? Ty as that -> eq that
        | _ -> false
