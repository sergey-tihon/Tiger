module Tiger.Types

open System
open Tiger

type Unique = obj ref

[<CustomEquality; NoComparison>]
type Ty =
    | RECORD of (Symbol.Symbol * Ty) list * Unique
    | NIL
    | INT
    | STRING
    | ARRAY of Ty * Unique
    | NAME of Symbol.Symbol * (Ty option) ref
    | UNIT

    override __.GetHashCode() = 0
    override this.Equals(thatObj) =
        let eq (that:Ty) =
            match this, that with
            | RECORD(_, x), RECORD(_,  y) -> x = y
            | NIL, NIL -> true
            | INT, INT -> true
            | STRING, STRING -> true
            | ARRAY(_,x), ARRAY(_,y) -> x=y
            | NAME(s1,t1), NAME(s2,t2) -> s1=s2 && !t1 = !t2
            | UNIT, UNIT -> true
            | _ -> false
        match thatObj with
        | :? Ty as that -> eq that
        | _ -> false
