module Interpreter

open Token
open Expr
open Error

let isTruthy =
    function
    | Nil -> false
    | Bool v -> v
    | _ -> true

exception TypeError

let typeError expected value line =
    Error.TypeError(value, expected, line)
    raise TypeError

let rec evaluate =
    function
    | Literal v -> v

    | Grouping expr -> evaluate expr

    | Unary ({ Type = TokenType.Minus; Line = line }, expr) ->
        match evaluate expr with
        | Number n -> Number -n
        | v -> typeError "number" v line

    | Unary ({ Type = TokenType.Bang }, expr) ->
        let right = evaluate expr
        Bool(not (isTruthy right))

    | Binary (left, { Type = TokenType.Plus; Line = line }, right) ->
        match evaluate left, evaluate right with
        | Number left, Number right -> Number(left + right)
        | String left, String right -> String(left + right)
        | Number _, badRight -> typeError "number" badRight line
        | String _, badRight -> typeError "string" badRight line
        | badLeft, Number _
        | badLeft, String _ -> typeError "number or string" badLeft line
        | _, badRight -> typeError "number or string" badRight line

    | Binary (left, { Type = TokenType.BangEqual }, right) ->
        let left, right = evaluate left, evaluate right
        Bool(left <> right)

    | Binary (left, { Type = TokenType.EqualEqual }, right) ->
        let left, right = evaluate left, evaluate right
        Bool(left = right)

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
        | badLeft, Number _ -> typeError "number" badLeft token.Line
        | _, badRight -> typeError "number" badRight token.Line

    | _ -> failwith "unreachable"
