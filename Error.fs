module Error

open Token
open Ast

[<AutoOpen>]
module Exceptions =
    exception ParseError of list<Token>
    exception RuntimeError of option<Literal> * string * int

let printError line where message =
    printfn $"[line %d{line}] Error%s{where}: %s{message}"

type Error() =
    static let mutable hadError = false

    static let report line where message =
        printError line where message
        hadError <- true

    static member Occurred() = hadError
    static member Reset() = hadError <- false

    static member Report(line, message) = report line "" message

    static member Report(token: Token, message) =
        if token.Type = Eof then
            report token.Line " at end" message
        else
            report token.Line $" at '%s{token.Lexeme}'" message

type RuntimeError() =
    static let mutable hadError = false

    static let report line where message =
        printError line where message
        hadError <- true

    static member Occurred() = hadError
    static member Reset() = hadError <- false

    static member Report(value: Literal, message, line: int) = report line $" at '%O{value}'" message
    static member Report(message, line: int) = report line "" message
