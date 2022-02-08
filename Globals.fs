module Globals

#nowarn "25"

open Ast
open System

let clock name env =
    Literal.Function(
        name,
        0,
        env,
        (fun [] env ->
            Number
            <| (float (DateTimeOffset.Now.ToUnixTimeMilliseconds()))
               / 1000.0)
    )
