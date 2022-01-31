module Lox

open System
open System.IO

open Error

let run source =
    let tokens =
        Scanner.make source |> Scanner.scanTokens

    match Parser.parse tokens with
    | Some expression ->
        let result = Interpreter.evaluate expression

        if not (Error.Occurred()) then
            printfn "%O\n" result
    | None -> ()

let runFile path =
    File.ReadAllText path |> run

    // Indicate an error in the exit code.
    if Error.Occurred() then exit 65

let rec runPrompt () =
    printf "> "

    match Console.ReadLine() |> Option.ofObj with
    | Some line ->
        try
            run line
        with
        | Interpreter.TypeError -> ()

        Error.Reset()
        runPrompt ()
    | None -> ()

[<EntryPoint>]
let main args =
    match args with
    | [||] -> runPrompt ()
    | [| file |] -> runFile file
    | _ ->
        printfn "Usage: flox [script]"
        exit 64

    0
