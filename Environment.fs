[<RequireQualifiedAccess>]
module Environment

open Ast
open Common
open Error
open Token

type Environment(env) =
    let mutable environment: Ast.Environment = env

    new() = Environment([ Map.empty ])

    member _.Get() = environment

    member _.Push() = environment <- Map.empty :: environment
    member _.Pop() = environment <- List.tail environment

    member _.DefineGlobal(name, fn) : unit =
        let rec loop =
            function
            | _, [] -> raise <| FatalError("No environment")
            | seen, [ env ] ->
                environment <-
                    List.rev seen
                    @ [ Map.add name (ref (fn name [ env ])) env ]
            | seen, env :: envs -> loop (env :: seen, envs)

        loop ([], environment)

    member _.Define(token, value) : unit =
        match environment with
        | [] -> raise <| FatalError("No environment.")
        | env :: envs -> environment <- Map.add token.Lexeme (ref value) env :: envs

    member _.Assign(token, value) : Literal =
        let rec loop =
            function
            | ([]: Ast.Environment) -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
            | env :: envs ->
                if (env.ContainsKey(token.Lexeme)) then
                    env.[token.Lexeme].Value <- value
                    value
                else
                    loop envs

        loop environment

    member _.Get(token) : Literal =
        let rec loop =
            function
            | ([]: Ast.Environment) -> runtimeError $"Undefined variable '%s{token.Lexeme}'." token.Line
            | env :: envs ->
                match Map.tryFind token.Lexeme env with
                | Some { contents = value } -> value
                | None -> loop envs

        loop environment

    member _.Get(name, line) : Literal =
        let rec loop =
            function
            | ([]: Ast.Environment) -> runtimeError $"Undefined variable '%s{name}'." line
            | env :: envs ->
                match Map.tryFind name env with
                | Some { contents = value } -> value
                | None -> loop envs

        loop environment
