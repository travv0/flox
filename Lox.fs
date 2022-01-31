module Lox

open System
open System.IO

open Error
open Parser

let run source =
    let tokens =
        Scanner.make source |> Scanner.scanTokens

    let parser = Parser(tokens)
    let expression = parser.Parse()

    if not (Error.Occurred()) then
        printfn "%A\n" expression

let runFile path =
    File.ReadAllText path |> run

    // Indicate an error in the exit code.
    if Error.Occurred() then exit 65

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
        printfn "Usage: flox [script]"
        exit 64

    0
