[<RequireQualifiedAccess>]
module Environment

open System.Collections.Generic
open Ast
open Error
open Token

type Environment = Dictionary<string, option<Literal>>

let make () = Environment()

let defineGlobal name value (env: Environment) =
    env.Add(name, Some value)
    env

let define token value (env: list<Environment>) =
    match env with
    | [] -> raise <| FatalError("No environment.")
    | e :: _ ->
        if not (e.TryAdd(token.Lexeme, value)) then
            e.[token.Lexeme] <- value

let rec assign token value (env: list<Environment>) =
    match env with
    | [] -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
    | e :: enclosing ->
        if (e.ContainsKey(token.Lexeme)) then
            e.[token.Lexeme] <- Some value
            value
        else
            assign token value enclosing

let rec get token (env: list<Environment>) =
    match env with
    | [] -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
    | e :: enclosing ->
        match e.TryGetValue(token.Lexeme) with
        | true, value -> value
        | false, _ -> get token enclosing
