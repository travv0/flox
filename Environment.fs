[<RequireQualifiedAccess>]
module Environment

open System.Collections.Generic
open Ast
open Error
open Token

type private Env = Dictionary<string, option<Literal>>
type Environment = Environment of list<Env>

let make () = Environment([ Env() ])
let extend (Environment env) = Environment(Env() :: env)

let rec defineGlobal name value (env: Environment) =
    match env with
    | Environment [] -> raise <| FatalError("No environment.")
    | Environment ([ e ]) ->
        e.Add(name, Some value)
        Environment [ e ]
    | Environment (e :: enclosing) ->
        let (Environment g) =
            defineGlobal name value (Environment enclosing)

        Environment(e :: g)

let define token value (env: Environment) =
    match env with
    | Environment [] -> raise <| FatalError("No environment.")
    | Environment (e :: _) ->
        if not (e.TryAdd(token.Lexeme, value)) then
            e.[token.Lexeme] <- value

let rec assign token value (env: Environment) =
    match env with
    | Environment [] -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
    | Environment (e :: enclosing) ->
        if (e.ContainsKey(token.Lexeme)) then
            e.[token.Lexeme] <- Some value
            value
        else
            assign token value (Environment enclosing)

let rec get token (env: Environment) =
    match env with
    | Environment [] -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
    | Environment (e :: enclosing) ->
        match e.TryGetValue(token.Lexeme) with
        | true, value -> value
        | false, _ -> get token (Environment enclosing)
