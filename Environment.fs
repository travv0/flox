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
        | true, Some value -> value
        | _, _ -> get token (Environment enclosing)

let rec getAt n token (Environment env) =
    List.skip n env
    |> fun env -> get token (Environment env)

let rec assignAt n token value (Environment env) =
    List.skip n env
    |> fun env -> assign token value (Environment env)

let rec getGlobal token (Environment env) =
    match env with
    | [] -> raise <| FatalError("No environment.")
    | [ e ] -> get token (Environment [ e ])
    | _ :: enclosing -> getGlobal token (Environment enclosing)

let rec assignGlobal token value (Environment env) =
    match env with
    | [] -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
    | [ e ] -> assign token value (Environment [ e ])
    | e :: enclosing -> assignGlobal token value (Environment enclosing)
