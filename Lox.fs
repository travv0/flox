module Lox

open System
open System.IO

open Common
open Error
open Interpreter

type RunType =
    | Interpret
    | Tokens
    | Ast

let interpreter = Interpreter()

let run runType source =
    let tokens = Scanner.scan source

    if runType = Tokens then
        if not (Error.Occurred()) then
            for token in tokens do
                printfn "%A" token
    else
        let ast = tokens |> Parser.parse

        if not (Error.Occurred()) then
            if runType = Ast then
                printfn "%A" ast
            else
                Analyzer.analyze ast

                if not (Error.Occurred()) then
                    interpreter.Interpret(ast)

let runFile runType path =
    File.ReadAllText path |> run runType

    // Indicate an error in the exit code.
    if Error.Occurred() then exit 65
    if RuntimeError.Occurred() then exit 70

let rec runPrompt () =
    printf "> "

    match Console.ReadLine() |> Option.ofObj with
    | Some line ->
        run Interpret line
        Error.Reset()
        runPrompt ()
    | None -> ()

[<EntryPoint>]
let main args =
    match args with
    | [||] -> runPrompt ()
    | [| file |] -> runFile Interpret file
    | [| "--ast"; file |] -> runFile Ast file
    | [| "--tokens"; file |] -> runFile Tokens file
    | _ ->
        eprintfn "Usage: flox [--tokens|--ast] [script]"
        exit 64

    0
