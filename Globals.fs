module Globals

#nowarn "25"

open Ast
open System

let clock =
    Function(
        0,
        (fun [] ->
            Number
            <| (float (DateTimeOffset.Now.ToUnixTimeMilliseconds()))
               / 1000.0)
    )
