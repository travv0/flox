module Error

let mutable private _hadError = false
let occurred () = _hadError
let reset () = _hadError <- false

let private report line where message =
    printfn $"[line %d{line}] Error%s{where}: %s{message}"
    _hadError <- true

let error line message = report line "" message
