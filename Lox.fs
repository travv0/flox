open Error
open System
open System.IO

module Lox =
    open Scanner

    let run source =
        let scanner = Scanner(source)
        let tokens = scanner.ScanTokens()

        // For now, just print the tokens.
        for token in tokens do
            printfn "%O" token

    let runFile path =
        File.ReadAllText path |> run

        // Indicate an error in the exit code.
        if hadError () then exit 65

    let rec runPrompt () =
        printf "> "

        match Console.ReadLine() |> Option.ofObj with
        | Some line ->
            run line
            resetError ()
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
