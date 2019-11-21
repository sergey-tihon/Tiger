module Tiger.Types 

open Tiger

type Unique = unit ref
type Ty = 
    | RECORD of (Symbol.Symbol * Ty) list * Unique
    | NIL
    | INT
    | STRING
    | ARRAY of Ty * Unique
    | NAME of Symbol.Symbol * Ty option ref
    | UNIT
