module Interpreter

open System

open Token
open Ast
open Error

let private equal left right =
    match left, right with
    | Number left, Number right when Double.IsNaN(left) && Double.IsNaN(right) -> true
    | _ -> left = right

let private isTruthy =
    function
    | Nil -> false
    | Bool v -> v
    | _ -> true

exception RuntimeError of Literal * string * int

let private runtimeError expected value line =
    raise <| RuntimeError(value, expected, line)

let rec private evaluate =
    function
    | Literal v -> v

    | Grouping expr -> evaluate expr

    | Unary ({ Line = line }, Minus, expr) ->
        match evaluate expr with
        | Number n -> Number -n
        | v -> runtimeError "number" v line

    | Unary (_, Bang, expr) ->
        let right = evaluate expr
        Bool(not (isTruthy right))

    | Binary ({ Line = line }, left, op, right) ->
        match evaluate left, op, evaluate right with
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

let private execute =
    function
    | Expression expr -> evaluate expr |> ignore
    | Print expr -> evaluate expr |> printfn "%O"

let interpret statements =
    try
        for statement in statements do
            execute statement
    with
    | RuntimeError (value, expected, line) -> RuntimeError.Report(value, expected, line)
