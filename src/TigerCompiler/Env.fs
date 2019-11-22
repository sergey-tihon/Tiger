module  Tiger.Env

open Tiger

module S = Symbol
module T = Types

type Access = unit (* TODO *)

type EnvEntry =
    | VarEntry of ty: T.Ty
    | FunEntry of formals: T.Ty list * result: T.Ty

type TEnv = Map<S.Symbol, T.Ty>
type VEnv = Map<S.Symbol, EnvEntry>

let private predefinedTypes = 
    [
    "int", Types.INT
    "string", Types.STRING
    "unit", Types.UNIT (* TODO: remove 'unit' type - this is a hack *)
    ]

let baseTEnv = 
    (S.empty, predefinedTypes)
    ||> List.fold (fun tenv (name, ty) ->
        tenv |> S.enter (S.symbol name) ty) 

(* TODO: add runtime library functions *)
let predefinedVars = 
    [
    "nil", VarEntry(ty=T.NIL)
    "print", FunEntry(formals=[T.STRING], result=T.UNIT)
    "flush", FunEntry(formals=[], result=T.UNIT)
    "getchar", FunEntry(formals=[], result=T.STRING)
    "ord", FunEntry(formals=[T.STRING], result=T.INT)
    "chr", FunEntry(formals=[T.INT], result=T.STRING)
    "size", FunEntry(formals=[T.STRING], result=T.INT)
    "substring", FunEntry(formals=[T.STRING; T.INT; T.INT], result=T.STRING)
    "concat", FunEntry(formals=[T.STRING; T.STRING], result=T.STRING)
    "not", FunEntry(formals=[T.INT], result=T.INT) (* TODO: Tiger doesn't include a boolean type. Would be useful here. *)
    "exit", FunEntry(formals=[T.INT], result=T.UNIT)
    ]

let baseVEnv =
    (S.empty, predefinedVars)
    ||> List.fold (fun venv (name, enventry) ->
        venv |> S.enter (S.symbol name) enventry) 