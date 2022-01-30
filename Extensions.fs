module Extensions

open System

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
