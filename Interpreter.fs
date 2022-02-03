module Interpreter

open System

open Token
open Ast
open Error

type Environment = Environment.Environment

let private equal left right =
    match left, right with
    | Number left, Number right when Double.IsNaN(left) && Double.IsNaN(right) -> true
    | _ -> left = right

let private isTruthy =
    function
    | Nil -> false
    | Bool v -> v
    | _ -> true

let private runtimeError expected value line =
    raise
    <| RuntimeError(Some value, $"Expect %s{expected}.", line)

let rec private evaluate (env: list<Environment>) =
    function
    | Literal v -> v

    | Variable token ->
        match Environment.get env token with
        | Some v -> v
        | None -> Nil

    | Assign (token, expr) -> evaluate env expr |> Environment.assign env token

    | Grouping expr -> evaluate env expr

    | Unary (({ Line = line }, Minus), expr) ->
        match evaluate env expr with
        | Number n -> Number -n
        | v -> runtimeError "number" v line

    | Unary ((_, Bang), expr) ->
        let right = evaluate env expr
        Bool(not (isTruthy right))

    | Binary (left, ({ Line = line }, op), right) ->
        match evaluate env left, op, evaluate env right with
        | Number left, BinaryOp.Minus, Number right -> Number(left - right)
        | Number left, Slash, Number right -> Number(left / right)
        | Number left, Star, Number right -> Number(left * right)

        | Number _, BinaryOp.Minus, badRight
        | Number _, Slash, badRight
        | Number _, Star, badRight -> runtimeError "number" badRight line

        | badLeft, BinaryOp.Minus, _
        | badLeft, Slash, _
        | badLeft, Star, _ -> runtimeError "number" badLeft line

        | Number left, Plus, Number right -> Number(left + right)
        | String left, Plus, String right -> String(left + right)

        | Number _, Plus, badRight -> runtimeError "number" badRight line
        | String _, Plus, badRight -> runtimeError "string" badRight line
        | badLeft, Plus, _ -> runtimeError "number or string" badLeft line

        | Number left, Greater, Number right -> Bool(left > right)
        | Number left, GreaterEqual, Number right -> Bool(left >= right)
        | Number left, Less, Number right -> Bool(left < right)
        | Number left, LessEqual, Number right -> Bool(left <= right)

        | Number _, Greater, badRight
        | Number _, GreaterEqual, badRight
        | Number _, Less, badRight
        | Number _, LessEqual, badRight -> runtimeError "number" badRight line

        | badLeft, Greater, _
        | badLeft, GreaterEqual, _
        | badLeft, Less, _
        | badLeft, LessEqual, _ -> runtimeError "number" badLeft line

        | left, BangEqual, right -> Bool(left <> right)
        | left, EqualEqual, right -> Bool(equal left right)

    | Logical (leftExpr, ({ Line = line }, And), rightExpr) ->
        let left = evaluate env leftExpr

        if isTruthy left then
            evaluate env rightExpr
        else
            left

    | Logical (leftExpr, ({ Line = line }, Or), rightExpr) ->
        let left = evaluate env leftExpr

        if isTruthy left then
            left
        else
            evaluate env rightExpr

let rec private execute (env: list<Environment>) =
    function
    | If (cond, thenBranch, elseBranch) ->
        if isTruthy (evaluate env cond) then
            execute env thenBranch
        else
            elseBranch |> Option.iter (execute env)
    | Expression expr -> evaluate env expr |> ignore
    | Print expr ->
        evaluate env expr
        |> fun v -> v.Display() |> printfn "%s"
    | While (cond, body) ->
        while isTruthy (evaluate env cond) do
            execute env body
    | Var (token, binding) ->
        binding
        |> Option.map (evaluate env)
        |> Environment.define env token
    | Block statements ->
        let env = Environment.make () :: env

        for statement in statements do
            execute env statement

let environment = Environment.make ()

let interpret statements =
    try
        for statement in statements do
            execute [ environment ] statement
    with
    | RuntimeError (value, expected, line) ->
        match value with
        | Some value -> RuntimeError.Report(value, expected, line)
        | None -> RuntimeError.Report(expected, line)
