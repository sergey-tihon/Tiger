module Tiger.Ast

open FSharp.Text.Lexing

type Pos = Position
type Symbol = string

type Var =
    | SimpleVar of symbol:Symbol * Pos
    | FieldVar of Var * Symbol * Pos
    | SubscriptVar of Var * Exp * Pos
and Exp =
    | VarExp of Var
    | NilExp
    | IntExp of int
    | StringExp of string * Pos
    | CallExp of func:Symbol * args:Exp list * pos:Pos
    | OpExp of left:Exp * oper:Oper * right:Exp * pos:Pos
    | RecordExp of fields:(Symbol * Exp * Pos) list * typ:Symbol * pos:Pos
    | SeqExp of (Exp * Pos) list
    | AssignExp of var:Var * exp:Exp * pos:Pos
    | IfExp of test:Exp * then':Exp * else':Exp option * pos:Pos
    | WhileExp of test:Exp * body:Exp * pos:Pos
    | ForExp of var:Symbol * escape: bool ref * lo: Exp * hi:Exp * body:Exp * pos:Pos
    | BreakExp of Pos
    | LetExp of decs: Dec list * body: Exp * pos:Pos
    | ArrayExp of typ:Symbol * size: Exp * init:Exp * pos:Pos
and Dec = 
    | FunctionDec of FunDec list
    | VarDec of name:Symbol * escape:bool ref * typ:(Symbol * Pos) option * init: Exp * pos: Pos
    | TypeDec of (Symbol * Ty * Pos) list
and Ty = 
    | NameTy  of Symbol * Pos
    | RecordTy of Field list
    | ArrayTy of Symbol * Pos
and Oper =
    | PlusOp | MinusOp | TimesOp | DivideOp
    | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
and Field = {
    name: Symbol
    escape: bool ref
    typ: Symbol
    pos: Pos}
and FunDec = {
    name: Symbol
    params': Field list
    result: (Symbol * Pos) option
    body: Exp
    pos: Pos}
