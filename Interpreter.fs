module Interpreter

open System

open Token
open Expr
open Error

let isTruthy =
    function
    | Nil -> false
    | Bool v -> v
    | _ -> true

exception RuntimeError of Literal * string * int

let runtimeError expected value line =
    raise <| RuntimeError(value, expected, line)

let rec private evaluate =
    function
    | Literal v -> v

    | Grouping expr -> evaluate expr

    | Unary ({ Type = TokenType.Minus; Line = line }, expr) ->
        match evaluate expr with
        | Number n -> Number -n
        | v -> runtimeError "number" v line

    | Unary ({ Type = TokenType.Bang }, expr) ->
        let right = evaluate expr
        Bool(not (isTruthy right))

    | Binary (left, { Type = TokenType.Plus; Line = line }, right) ->
        match evaluate left, evaluate right with
        | Number left, Number right -> Number(left + right)
        | String left, String right -> String(left + right)
        | Number _, badRight -> runtimeError "number" badRight line
        | String _, badRight -> runtimeError "string" badRight line
        | badLeft, Number _
        | badLeft, String _ -> runtimeError "number or string" badLeft line
        | _, badRight -> runtimeError "number or string" badRight line

    | Binary (left, { Type = TokenType.BangEqual }, right) ->
        let left, right = evaluate left, evaluate right
        Bool(left <> right)

    | Binary (left, { Type = TokenType.EqualEqual }, right) ->
        let equal left right =
            match left, right with
            | Number left, Number right when Double.IsNaN(left) && Double.IsNaN(right) -> true
            | _ -> left = right

        let left, right = evaluate left, evaluate right
        Bool(equal left right)

    | Binary (left, token, right) ->
        match evaluate left, evaluate right with
        | Number left, Number right ->
            match token.Type with
            | TokenType.Minus -> Number(left - right)
            | TokenType.Slash -> Number(left / right)
            | TokenType.Star -> Number(left * right)
            | TokenType.Greater -> Bool(left > right)
            | TokenType.GreaterEqual -> Bool(left >= right)
            | TokenType.Less -> Bool(left < right)
            | TokenType.LessEqual -> Bool(left <= right)
            | _ -> failwith "unreachable"
        | badLeft, Number _ -> runtimeError "number" badLeft token.Line
        | _, badRight -> runtimeError "number" badRight token.Line

    | _ -> failwith "unreachable"

let interpret expression =
    try
        expression |> evaluate |> printfn "%O"
    with
    | RuntimeError (value, expected, line) -> RuntimeError.Report(value, expected, line)
