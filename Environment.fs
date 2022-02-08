[<RequireQualifiedAccess>]
module Environment

open Ast
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
        if List.length environment > 1
           && Map.containsKey token.Lexeme (List.head environment) then
            runtimeError $"Already a variable named '%s{token.Lexeme}' in this scope." token.Line

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
                match env.TryGetValue(token.Lexeme) with
                | true, { contents = value } -> value
                | false, _ -> loop envs

        loop environment
