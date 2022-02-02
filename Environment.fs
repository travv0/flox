module Environment

open System.Collections.Generic
open Ast
open Error
open Token

let make () = Dictionary<string, option<Literal>>()

let define (values: Dictionary<_, _>) token value =
    if not (values.TryAdd(token.Lexeme, value)) then
        values.[token.Lexeme] <- value

let assign (values: Dictionary<_, _>) token value =
    if (values.ContainsKey(token.Lexeme)) then
        values.[token.Lexeme] <- Some value
        value
    else
        raise
        <| RuntimeError(None, $"Undefined variable '%s{token.Lexeme}'.", token.Line)

let get (values: Dictionary<_, _>) token =
    match values.TryGetValue(token.Lexeme) with
    | true, value -> value
    | false, _ ->
        raise
        <| RuntimeError(None, $"Undefined variable '%s{token.Lexeme}'.", token.Line)
