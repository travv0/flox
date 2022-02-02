[<RequireQualifiedAccess>]
module Environment

open System.Collections.Generic
open Ast
open Error
open Token

type Environment = Dictionary<string, option<Literal>>

let make () = Environment()

let define (env: list<Environment>) token value =
    match env with
    | [] -> raise <| FatalError("No environment.")
    | e :: _ ->
        if not (e.TryAdd(token.Lexeme, value)) then
            e.[token.Lexeme] <- value

let rec assign (env: list<Environment>) token value =
    match env with
    | [] ->
        raise
        <| RuntimeError(None, $"Undefined variable '%s{token.Lexeme}'.", token.Line)
    | e :: enclosing ->
        if (e.ContainsKey(token.Lexeme)) then
            e.[token.Lexeme] <- Some value
            value
        else
            assign enclosing token value

let rec get (env: list<Environment>) token =
    match env with
    | [] ->
        raise
        <| RuntimeError(None, $"Undefined variable '%s{token.Lexeme}'.", token.Line)
    | e :: enclosing ->
        match e.TryGetValue(token.Lexeme) with
        | true, value -> value
        | false, _ -> get enclosing token
