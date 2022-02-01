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

type BinaryOp =
    | Plus
    | BangEqual
    | EqualEqual
    | Minus
    | Slash
    | Star
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

module BinaryOp =
    let ofToken { Type = type_ } =
        match type_ with
        | TokenType.BangEqual -> Some BangEqual
        | TokenType.EqualEqual -> Some EqualEqual
        | TokenType.Greater -> Some Greater
        | TokenType.GreaterEqual -> Some GreaterEqual
        | TokenType.Less -> Some Less
        | TokenType.LessEqual -> Some LessEqual
        | TokenType.Minus -> Some Minus
        | TokenType.Plus -> Some Plus
        | TokenType.Slash -> Some Slash
        | TokenType.Star -> Some Star

        | TokenType.And
        | TokenType.Bang
        | TokenType.Class
        | TokenType.Comma
        | TokenType.Dot
        | TokenType.Else
        | TokenType.Equal
        | TokenType.Eof
        | TokenType.False
        | TokenType.For
        | TokenType.Fun
        | TokenType.Identifier _
        | TokenType.If
        | TokenType.LeftBrace
        | TokenType.LeftParen
        | TokenType.Nil
        | TokenType.Number _
        | TokenType.Or
        | TokenType.Print
        | TokenType.Return
        | TokenType.RightBrace
        | TokenType.RightParen
        | TokenType.Semicolon
        | TokenType.String _
        | TokenType.Super
        | TokenType.This
        | TokenType.True
        | TokenType.Var
        | TokenType.While -> None

type UnaryOp =
    | Minus
    | Bang

type Expr =
    | Binary of Token * Expr * BinaryOp * Expr
    | Unary of Token * UnaryOp * Expr
    | Literal of Literal
    | Grouping of Expr
