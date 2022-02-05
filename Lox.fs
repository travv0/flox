module Lox

open System
open System.IO

open Error

let run source =
    let tokens =
        Scanner.make source |> Scanner.scanTokens

    Parser.parse tokens
    |> Option.iter Interpreter.interpret

let runFile path =
    File.ReadAllText path |> run

    // Indicate an error in the exit code.
    if Error.Occurred() then exit 65
    if RuntimeError.Occurred() then exit 70

let rec runPrompt () =
    printf "> "

    match Console.ReadLine() |> Option.ofObj with
    | Some line ->
        run line
        Error.Reset()
        runPrompt ()
    | None -> ()

[<EntryPoint>]
let main args =
    match args with
    | [||] -> runPrompt ()
    | [| file |] -> runFile file
    | _ ->
        eprintfn "Usage: flox [script]"
        exit 64

    0
