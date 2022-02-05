module Globals

#nowarn "25"

open Ast
open System

let clock =
    Literal.Function(
        "clock",
        0,
        (fun [] ->
            Number
            <| (float (DateTimeOffset.Now.ToUnixTimeMilliseconds()))
               / 1000.0)
    )
