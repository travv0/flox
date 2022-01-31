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
        | Class
        | Fun
        | Var
        | For
        | If
        | While
        | Print
        | Return -> true
        | _ -> false

    let rest =
        List.skipWhile
            (fun token ->
                token.Type <> Semicolon
                && not (isStatementStart token))
            tokens

    match rest with
    | { Type = Semicolon } :: rest -> rest
    | _ -> rest

module private Grammar =
    type ParseResult = Expr * list<Token>

    let rec expression tokens : ParseResult = equality tokens

    and primary: list<Token> -> ParseResult =
        function
        | { Type = False } :: rest -> Literal(false), rest
        | { Type = True } :: rest -> Literal(true), rest
        | { Type = Nil } :: rest -> Literal(null), rest
        | { Type = type_; Literal = literal } :: rest when List.contains type_ [ Number; String ] ->
            Literal(literal), rest
        | { Type = LeftParen } :: rest ->
            let expr, rest = expression rest
            Grouping(expr), consume rest RightParen "Expect ')' after expression."
        | token :: _ -> raise <| parseError token "Expect expression."
        | _ ->
            raise
            <| parseError (eof 0) "Unexpected end of file."

    and unary: list<Token> -> ParseResult =
        function
        | operator :: rest when List.contains operator.Type [ Bang; Minus ] ->
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

    and factor = binary unary [ Slash; Star ]

    and term = binary factor [ Minus; Plus ]

    and comparison =
        binary
            term
            [ Greater
              GreaterEqual
              Less
              LessEqual ]

    and equality =
        binary comparison [ BangEqual; EqualEqual ]

let parse tokens =
    try
        tokens
        |> List.ofSeq
        |> Grammar.expression
        |> fst
        |> Some
    with
    | ParseError -> None
