open System
open System.IO

module Lox =
    let run source =
        let tokens =
            Scanner.make source |> Scanner.scanTokens

        // For now, just print the tokens.
        for token in tokens do
            printfn "%O" token

        printfn ""

    let runFile path =
        File.ReadAllText path |> run

        // Indicate an error in the exit code.
        if Error.occurred () then exit 65

    let rec runPrompt () =
        printf "> "

        match Console.ReadLine() |> Option.ofObj with
        | Some line ->
            run line
            Error.reset ()
            runPrompt ()
        | None -> ()

[<EntryPoint>]
let main args =
    match args with
    | [||] -> Lox.runPrompt ()
    | [| file |] -> Lox.runFile file
    | _ ->
        printfn "Usage: flox [script]"
        exit 64

    0
