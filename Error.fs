module Error

open Token

let mutable _hadError = false

let report line where message =
    printfn $"[line %d{line}] Error%s{where}: %s{message}"
    _hadError <- true

type Error() =
    static member Occurred() = _hadError
    static member Reset() = _hadError <- false

    static member Report(line, message) = report line "" message

    static member Report(token: Token, message) =
        if token.Type = Eof then
            report token.Line " at end" message
        else
            report token.Line $" at '%s{token.Lexeme}'" message
