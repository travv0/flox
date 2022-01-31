module Parser

open Error
open Token
open Expr

exception ParseError

let private parseError (token: Token) message =
    Error.Report(token, message)
    ParseError

let private consume tokens type_ message =
    match tokens with
    | token :: rest when token.Type = type_ -> rest
    | token :: _ -> raise <| parseError token message
    | _ ->
        raise
        <| parseError (eof 0) "Unexpected end of file."

let private synchronize tokens : list<Token> =
    let isStatementStart token =
        match token.Type with
        | TokenType.Class
        | TokenType.Fun
        | TokenType.Var
        | TokenType.For
        | TokenType.If
        | TokenType.While
        | TokenType.Print
        | TokenType.Return -> true
        | _ -> false

    let rest =
        List.skipWhile
            (fun token ->
                token.Type <> TokenType.Semicolon
                && not (isStatementStart token))
            tokens

    match rest with
    | { Type = TokenType.Semicolon } :: rest -> rest
    | _ -> rest

module private Grammar =
    type ParseResult = Expr * list<Token>

    let rec expression tokens : ParseResult = equality tokens

    and primary: list<Token> -> ParseResult =
        function
        | { Type = TokenType.False } :: rest -> Literal(Bool false), rest
        | { Type = TokenType.True } :: rest -> Literal(Bool true), rest
        | { Type = TokenType.Nil } :: rest -> Literal Nil, rest
        | { Type = TokenType.String
            Literal = literal } :: rest -> Literal(String(literal :?> string)), rest
        | { Type = TokenType.Number
            Literal = literal } :: rest -> Literal(Number(literal :?> float)), rest
        | { Type = TokenType.LeftParen } :: rest ->
            let expr, rest = expression rest
            Grouping(expr), consume rest TokenType.RightParen "Expect ')' after expression."
        | token :: _ -> raise <| parseError token "Expect expression."
        | _ ->
            raise
            <| parseError (eof 0) "Unexpected end of file."

    and unary: list<Token> -> ParseResult =
        function
        | operator :: rest when List.contains operator.Type [ TokenType.Bang; TokenType.Minus ] ->
            let expr, rest = expression rest
            Unary(operator, expr), rest
        | token -> primary token

    and binary nextPrec opTokens tokens : ParseResult =
        let rec go expr =
            function
            | operator :: rest when List.contains operator.Type opTokens ->
                let right, rest = nextPrec rest
                go (Binary(expr, operator, right)) rest
            | rest -> expr, rest

        let expr, rest = nextPrec tokens
        go expr rest

    and factor =
        binary unary [ TokenType.Slash; TokenType.Star ]

    and term =
        binary factor [ TokenType.Minus; TokenType.Plus ]

    and comparison =
        binary
            term
            [ TokenType.Greater
              TokenType.GreaterEqual
              TokenType.Less
              TokenType.LessEqual ]

    and equality =
        binary
            comparison
            [ TokenType.BangEqual
              TokenType.EqualEqual ]

let parse tokens =
    try
        tokens
        |> List.ofSeq
        |> Grammar.expression
        |> fst
        |> Some
    with
    | ParseError -> None
