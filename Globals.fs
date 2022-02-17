module Globals

#nowarn "25"

open System.IO

open Error
open Ast
open System

let clock name env =
    Literal.Function(
        LoxFunction(
            name,
            0,
            FunctionType.Function,
            env,
            (fun [] env line ->
                Literal.Number
                <| (float (DateTimeOffset.Now.ToUnixTimeMilliseconds()))
                   / 1000.0)
        )
    )

let listDir name env =
    Literal.Function(
        LoxFunction(
            name,
            1,
            FunctionType.Function,
            env,
            (fun [ dir ] env line ->
                match dir with
                | Literal.String dir ->
                    for f in Directory.EnumerateFileSystemEntries(dir) do
                        printfn "%s" f
                | _ -> typeError "string" dir line

                Literal.Nil)
        )
    )
