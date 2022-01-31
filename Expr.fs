module Expr

open Token

type Literal =
    | Bool of bool
    | String of string
    | Number of float
    | Nil

    override this.ToString() =
        match this with
        | Bool true -> "true"
        | Bool false -> "false"
        | String s -> $"\"%s{s}\""
        | Number n -> string n
        | Nil -> "nil"

type Expr =
    | Binary of Expr * Token * Expr
    | Unary of Token * Expr
    | Literal of Literal
    | Grouping of Expr
