module Error

open Token
open Expr

let mutable _hadError = false

let report line where message =
    printfn $"[line %d{line}] Error%s{where}: %s{message}"
    _hadError <- true

type Error() =
    static member Occurred() = _hadError
    static member Reset() = _hadError <- false

    static member Report(line, message) = report line "" message

    static member Report(token: Token, message) =
        if token.Type = TokenType.Eof then
            report token.Line " at end" message
        else
            report token.Line $" at '%s{token.Lexeme}'" message

    static member TypeError(value: Literal, expected, line: int) =
        report line $" at '%O{value}'" $"Expect %s{expected}"
