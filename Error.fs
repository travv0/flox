module Error

let mutable private _hadError = false
let hadError () = _hadError
let resetError () = _hadError <- false

let private report line where message =
    printfn $"[line %d{line}] Error%s{where}: %s{message}"
    _hadError <- true

let error line message = report line "" message
