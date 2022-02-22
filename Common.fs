module Common

open System
open System.Collections.Generic

module String =
    let ofSeq source = source |> Seq.toArray |> String

module List =
    let splitWhile predicate list =
        let rec go list taken =
            match list with
            | [] -> (List.rev taken, [])
            | elem :: l when predicate elem -> go l (elem :: taken)
            | _ -> (List.rev taken, list)

        go list []

    let drop n list =
        if List.length list >= n then
            List.skip n list
        else
            []

type Dictionary<'K, 'V> with
    member this.TryFind(key) =
        match this.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

[<AutoOpen>]
module Operators =
    [<RequiresExplicitTypeArguments>]
    /// <summary>
    /// Ignore the passed value. This is often used to throw away results of a computation.
    /// </summary>
    /// <param name="value">The value to ignore</param>
    /// <typeparam name="'a">the type of value to expect, and ignore</typeparam>
    /// <returns>unit</returns>
    let ignore<'a> (value: 'a) = ignore<'a> value
