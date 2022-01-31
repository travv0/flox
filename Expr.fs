module Expr

open Token

type Expr =
    | Binary of Expr * Token * Expr
    | Unary of Token * Expr
    | Literal of obj
    | Grouping of Expr
