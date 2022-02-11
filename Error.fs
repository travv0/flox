module Error

open Token
open Ast

[<AutoOpen>]
module Exceptions =
    exception FatalError of string
    exception ParseError of list<Token>
    exception AnalysisError
    exception RuntimeError of option<Literal> * string * int

let printError (line: option<int>) where message =
    let location =
        match line with
        | Some line -> $"line %d{line}"
        | None -> "unknown location"

    eprintfn $"[%s{location}] Error%s{where}: %s{message}"

type Error() =
    static let mutable hadError = false

    static let report line where message =
        printError line where message
        hadError <- true

    static member Occurred() = hadError
    static member Reset() = hadError <- false

    static member Report(line, message) = report line "" message

    static member Report(token: option<Token>, message) =
        match token with
        | Some token ->
            if token.Type = Eof then
                report (Some token.Line) " at end" message
            else
                report (Some token.Line) $" at '%s{token.Lexeme}'" message
        | None -> report None "" message

type RuntimeError() =
    static let mutable hadError = false

    static let report line where message =
        printError line where message
        hadError <- true

    static member Occurred() = hadError
    static member Reset() = hadError <- false

    static member Report(value: Literal, message, line: int) =
        report (Some line) $" at '%O{value}'" message

    static member Report(message, line: int) = report (Some line) "" message

let runtimeError message line =
    raise
    <| Exceptions.RuntimeError(None, message, line)
