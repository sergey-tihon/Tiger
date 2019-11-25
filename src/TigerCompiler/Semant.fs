module Tiger.Semant

module A = Absyn
//module P = PrintAbsyn
module S = Symbol
module E = Env
module T = Types

// FIXME: Loops in type aliases cause non-halting type check phase
// FIXME: Reject multiple definitions with same type name. When consecutive only - otherwise shadows.
// FIXME: Reject multiple definitions with same variable name? or shadows?

// TODO: Consider adding boolean type to the language

module Translate =
    type Exp = unit
type ExpTy = {exp: Translate.Exp; ty: T.Ty}

let todoTy = T.INT
let todoTrExp = ()
let todoExpTy = {exp=todoTrExp; ty=todoTy}

// Value to use when expression is in error and no better value/ty can be provided
let errorTrExpTy = {exp=todoTrExp; ty=T.NIL}
let errorTrExp = ()

let wouldBeBooleanTy = T.INT

let error : A.Pos -> string -> unit = //ErrorMsg.error
    fun pos msg -> failwithf "Error:%s at (%A)" msg pos

let rec lookupActualType(pos, tenv, ty) =
    match S.look(tenv,ty) with
    | Some ty -> actualTy(pos, tenv, ty)
    | None    -> (error pos (sprintf "Type '%s' is not defined" (S.name ty)); T.NIL)
and actualTy(pos, tenv, ty) =
    match ty with
    | T.NAME (sym, ty) when !ty = None -> (error pos (sprintf "type '%s' is still None" (S.name sym)); T.NIL)
    | T.NAME (sym, ty) -> actualTy(pos, tenv, (!ty).Value)
    | _ -> ty

let checkInt(expty:ExpTy, pos) =
    match expty.ty with
    | T.INT -> ()
    | _ -> error pos "Type 'int' required"

let checkUnit(expty:ExpTy, pos) =
    match expty.ty with
    | T.UNIT -> ()
    | _ -> error pos "unit required"

// TODO: allow records to be compatible with NIL from either left or right?
let reqSameType(pos, tenv, expty1:ExpTy, expty2:ExpTy) =
    let t1 = actualTy(pos, tenv, expty1.ty)
    let t2 = actualTy(pos, tenv, expty2.ty)
    if t1 = t2 then () // FIXME: StackOverflow !!!
    else
        match t1 with
        | T.RECORD _ -> if t2 = T.NIL then () else (error pos "types do not match"; ())
        | _          -> error pos "types do not match"

let rec findVarType(tenv, venv, var:A.Var) =
    match var with
    | A.SimpleVar (sym, pos) ->
        match S.look(venv,sym) with
        | Some(E.VarEntry(ty)) -> actualTy (pos, tenv, ty)
        | Some(E.FunEntry _) -> (error pos "Cannot assign to a function"; T.NIL)
        | _ -> (error pos "Variable does not exist"; T.NIL)
    | A.FieldVar (var, sym, pos) ->
        (* Lookup type of nested var. It should be a record type. *)
        match findVarType(tenv, venv, var)  with
        | T.RECORD (fields, unique) ->
            match fields |> List.tryFind (fst >> ((=) sym))  with
            | Some(sym, ty) -> actualTy (pos, tenv, ty)
            | None          -> (error pos (sprintf "field %s does not exist" (S.name sym)); T.NIL)
        | _ -> (error pos "Variable is not a record"; T.NIL)
    | A.SubscriptVar (var, exp, pos) ->
        (* Lookup type of nested var. It should be an array type. *)
        match findVarType(tenv, venv, var) with
        | T.ARRAY (ty, unique) ->
            let expA = transExp(tenv, venv, exp)
            reqSameType(pos, tenv, expA, {exp=todoTrExp; ty=T.INT})
            actualTy (pos, tenv, ty)
        | _ -> (error pos "Variable is not an array"; T.NIL)

and transVar(tenv, venv, var) = todoExpTy

and transDec(tenv, venv, dec:A.Dec) =
    match dec with
    | A.VarDec(name, escape, None, init, pos) ->
        let tExp = transExp(tenv, venv, init)
        tenv, S.enter(venv, name, E.VarEntry tExp.ty)

    | A.VarDec(name, escape, Some(symbol, decTyPos), init, pos) ->
        let tExp = transExp(tenv, venv, init)
        let decTy = lookupActualType(pos, tenv, symbol)
        reqSameType(pos, tenv, {exp=todoTrExp; ty=decTy}, {exp=todoTrExp; ty=tExp.ty});
        tenv, S.enter(venv, name, E.VarEntry decTy) (* continue with declared type *)

    | A.TypeDec typeDecs ->
        let updateDecs (tenv, venv) =
            let updateDec (name, ty, pos) =
                let lookupType(pos, tenv, ty) =
                    match S.look(tenv, ty) with
                    | Some ty -> ty
                    | None -> (error pos (sprintf "Type '%s' is not defined" (S.name ty)); T.NIL)
                //val T.NAME(tyName, tyRef) = lookupType (pos, tenv, name)
                let ty =
                    match ty with
                    | A.NameTy (name, pos) ->
                        T.NAME (name, ref (Some (lookupType (pos, tenv, name))))
                    | A.RecordTy fields ->
                        let fields' = fields |> List.map (fun (f:A.Field) -> (f.name, lookupType (f.pos, tenv, f.typ)))
                        T.RECORD (fields', ref <| obj())
                    | A.ArrayTy (name, pos) ->
                        T.ARRAY (lookupType (pos, tenv, name), ref <| obj())
                match lookupType (pos, tenv, name) with
                | T.NAME(tyName, tyRef) -> tyRef := Some(ty)
                | _ -> ()
            //app updateDec typeDecs
            typeDecs |> List.iter (updateDec)
        let tenv' =
            (tenv, typeDecs)
            ||> List.fold (fun tenv (name, ty, pos) ->
                S.enter (tenv, name, T.NAME (name, ref None))
            )

        updateDecs (tenv', venv)
        tenv', venv

    | A.FunctionDec funDecs ->
        let computeResultType (tenv, result) =
            match result with
            | Some (resTySym, resPos) -> lookupActualType (resPos, tenv, resTySym)
            | None -> T.UNIT

        let transFunDecs(tenv, venv, funDecs) =
            let transFunDec(fn:A.FunDec) = //{name, params, body, pos, result}
                let resTy = computeResultType (tenv, fn.result)
                let params' =
                    fn.params' |> List.map (fun (f:A.Field) ->
                        (f.name, lookupActualType(f.pos, tenv, f.typ)))
                let venv' =
                    (venv, params')
                    ||> List.fold (fun venv (name, ty) ->
                        S.enter(venv, name, E.VarEntry ty))

                let bodyA = transExp(tenv, venv', fn.body);
                reqSameType(fn.pos, tenv, bodyA, {exp=todoTrExp; ty=resTy})
            //app transFunDec funDecs
            funDecs |> Seq.iter transFunDec

        let venv' =
            (venv, funDecs)
            ||> List.fold (fun venv (fn:A.FunDec) ->
                let resTy = computeResultType (tenv, fn.result)
                let params' =
                    fn.params' |> List.map (fun (f:A.Field) ->
                        lookupActualType(f.pos, tenv, f.typ))
                S.enter(venv, fn.name, E.FunEntry(params', resTy))
              )

        transFunDecs (tenv, venv', funDecs)
        tenv, venv'

and transDecs(tenv, venv, decs:A.Dec list) =
    match decs with
    | [] -> tenv, venv
    | dec::decs->
        let (tenv',venv') = transDec(tenv, venv, dec)
        transDecs(tenv', venv', decs)

and transExp(tenv, venv, exp) =
    let rec trexp (exp:A.Exp) =
        match exp with
        | A.NilExp -> {exp=todoTrExp; ty=T.UNIT}

        | A.VarExp var -> {exp=todoTrExp; ty=findVarType(tenv, venv, var)}

        | A.AssignExp(var, exp, pos) ->
            let expA = trexp exp
            let varTy = findVarType(tenv, venv, var)

            reqSameType(pos, tenv, {exp=todoTrExp; ty=varTy}, expA)
            {exp=todoTrExp; ty=T.UNIT}

        | A.ArrayExp(tySymbol, sizeExp, initExp, pos) ->
            let arrayTy = lookupActualType(pos, tenv, tySymbol)
            let sizeA = trexp sizeExp
            let initA = trexp initExp

            checkInt(sizeA, pos)
            match arrayTy with
            | T.ARRAY (ty, unique) ->
                reqSameType(pos, tenv, {exp=todoTrExp; ty=ty}, initA)
                {exp=todoTrExp; ty=arrayTy}
            | _ ->
                error pos "type is not an array"
                {exp=todoTrExp; ty=T.UNIT}

        | A.ForExp(varSym, escape, lo, hi, body, pos) ->
            let loA = trexp lo
            let hiA = trexp hi
            let venv'=S.enter(venv, varSym, E.VarEntry T.INT) (* declare loop variable *)
            (* TODO: ensure loop variable is not assigned to *)
            let bodyA = transExp (tenv, venv', body)

            checkInt(loA, pos)
            checkInt(hiA, pos)
            checkUnit(bodyA, pos)
            {exp=todoTrExp; ty=T.UNIT}

        | A.WhileExp(testExp, bodyExp, pos) ->
            let testA = trexp testExp
            let bodyA = trexp bodyExp

            checkInt(testA, pos)
            checkUnit(bodyA, pos)
            {exp=todoTrExp; ty=T.UNIT}

        | A.CallExp(func, args, pos) ->
            match S.look(venv, func) with
            | Some(E.VarEntry _) -> (error pos "Variable is not a function"; errorTrExpTy)
            | None -> (error pos "Function does not exist"; errorTrExpTy)
            | Some(E.FunEntry(formals,resTy)) ->
                let formalsN = List.length formals
                let actualsN = List.length args

                if formalsN <> actualsN
                then (error pos "Function has the wrong arity"; errorTrExpTy)
                else
                    (* check the type of each argument expression matches the corresponding formal parameter type *)
                    List.zip args formals
                    |> List.iter (fun (argExp, fTy) ->
                        reqSameType(pos, tenv, trexp argExp, {exp=todoTrExp; ty=fTy}))
                    {exp=todoTrExp; ty=resTy}

        | A.IfExp(testExp, thenExp, Some elseExp, pos) ->
            let testA = trexp testExp
            let thenA = trexp thenExp
            let elseA = trexp elseExp

            checkInt(testA, pos);
            reqSameType(pos, tenv, thenA, elseA);
            {exp=todoTrExp; ty=thenA.ty}

        | A.IfExp(testExp, thenExp, None, pos) ->
            let testA = trexp testExp
            let thenA = trexp thenExp

            checkInt(testA, pos)
            checkUnit(thenA, pos)
            {exp=todoTrExp; ty=T.UNIT}

        | A.RecordExp(fields, typ, pos) ->
            (* FIXME: Oops, I've required the fields in the record expression to be specified in the same order as in the
                      record declaration! *)
            let checkField ((symbol, exp, pos),(tySymbol, ty)) =
                let expA = trexp exp
                if symbol = tySymbol
                then reqSameType(pos, tenv, {exp=todoTrExp; ty=ty}, expA)
                else error pos "field is not in record type"

            let checkFields(tyFields) =
              if List.length fields = List.length tyFields
              then List.zip fields tyFields |> List.iter checkField
              else error pos "Record expression has the wrong arity"

            let t = lookupActualType(pos, tenv, typ)
            match t with
            | T.RECORD (tyFields, unique) -> (checkFields (tyFields); {exp=todoTrExp; ty=t})
            | _                           -> (error pos "Not a record type"; errorTrExpTy)

        | A.IntExp _ -> {exp=todoTrExp; ty=T.INT}
        | A.StringExp _ -> {exp=todoTrExp; ty=T.STRING}
        | A.BreakExp pos -> {exp=todoTrExp; ty=T.UNIT}

        | A.OpExp (left, oper, right, pos) ->
            let leftA = trexp left
            let rightA = trexp right
            let leftTy = actualTy (pos, tenv, leftA.ty) (* XXX: Not required? *)
            let rightTy = actualTy (pos, tenv, rightA.ty) (* XXX: Not required? *)

            (* The following are int-only operations: + - * / < > <= >= *)
            match oper with
            | A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp ->
                checkInt(leftA, pos)
                checkInt(rightA, pos)
                {exp=todoTrExp; ty=T.INT}
            | A.LtOp | A.LeOp | A.GtOp | A.GeOp ->
                checkInt(leftA, pos);
                checkInt(rightA, pos);
                {exp=todoTrExp; ty=wouldBeBooleanTy}
            | A.EqOp | A.NeqOp ->
                (* Operators = and <> operate on int, string, record and arrays. *)
                // TODO: Refactor checking of unique type for records/arrays.
                match (leftTy, rightTy) with
                | (T.INT, T.INT) -> {exp=todoTrExp; ty=wouldBeBooleanTy}
                | (T.STRING, T.STRING) -> {exp=todoTrExp; ty=wouldBeBooleanTy}
                | (T.RECORD (_, lUnique), T.RECORD (_, rUnique)) ->
                     if lUnique <> rUnique then
                       (error pos "Record types are not equal"; errorTrExpTy)
                     else {exp=todoTrExp; ty=wouldBeBooleanTy}
                | (T.NIL, T.RECORD _) -> {exp=todoTrExp; ty=wouldBeBooleanTy}
                | (T.RECORD _, T.NIL) -> {exp=todoTrExp; ty=wouldBeBooleanTy}
                | (T.ARRAY (lTy, lUnique), T.ARRAY (rTy, rUnique)) ->
                     if lUnique <> rUnique then
                       (error pos "Array types are not equal"; errorTrExpTy)
                     else {exp=todoTrExp; ty=wouldBeBooleanTy}
                | _ -> (error pos "Types mismatch"; errorTrExpTy)

        | A.LetExp(decs, body, pos) ->
            let (tenv',venv') = transDecs(tenv, venv, decs)
            transExp(tenv',venv', body)

        | A.SeqExp expList ->
            let rs = expList |> List.map (fst >> trexp)
            let lastExpTy = List.last rs
            {exp=todoTrExp; ty=lastExpTy.ty}

    and trvar(var:A.Var) =
        match var with
        | A.SimpleVar(id, pos)  ->
            match S.look(venv, id) with
            | Some(E.VarEntry(ty)) -> {exp=todoTrExp; ty=actualTy(pos, tenv, ty)}
            | Some(E.FunEntry _)   ->
                error pos (sprintf "variable points to a function: %s" (S.name id))
                {exp=errorTrExp; ty=T.INT}
            | None ->
                error pos (sprintf "undefined variable %s" (S.name id))
                {exp=errorTrExp; ty=T.INT}
        | A.FieldVar(var, sym, pos) -> todoExpTy
        | A.SubscriptVar(var, exp, pos) -> todoExpTy

    trexp(exp)

and transTy (tenv: E.TEnv, ty: A.Ty): T.Ty = todoTy (* XXX: what's this? *)

and transProg(exp: A.Exp) :unit =
    transExp(E.baseTEnv, E.baseVEnv, exp)
    |> ignore
