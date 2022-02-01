module Parser

open Error
open Token
open Ast

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
    | { Type = Semicolon } :: rest -> rest
    | _ -> rest

module private Grammar =
    type StmtResult = Stmt * list<Token>
    type ExprResult = Expr * list<Token>

    let rec statement tokens : StmtResult =
        match tokens with
        | { Type = TokenType.Print } :: rest -> printStatement rest
        | _ -> expressionStatement tokens

    and printStatement tokens : StmtResult =
        let value, rest = expression tokens
        Stmt.Print(value), consume rest Semicolon "Expect ';' after value."

    and expressionStatement tokens : StmtResult =
        let expr, rest = expression tokens
        Stmt.Expression(expr), consume rest Semicolon "Expect ';' after expression."

    and expression tokens : ExprResult = equality tokens

    and primary: list<Token> -> ExprResult =
        function
        | { Type = False } :: rest -> Literal(Bool false), rest
        | { Type = True } :: rest -> Literal(Bool true), rest
        | { Type = TokenType.Nil } :: rest -> Literal Nil, rest
        | { Type = TokenType.String s } :: rest -> Literal(String s), rest
        | { Type = TokenType.Number n } :: rest -> Literal(Number n), rest
        | { Type = TokenType.LeftParen } :: rest ->
            let expr, rest = expression rest
            Grouping(expr), consume rest TokenType.RightParen "Expect ')' after expression."
        | token :: _ -> raise <| parseError token "Expect expression."
        | _ ->
            raise
            <| parseError (eof 0) "Unexpected end of file."

    and unary: list<Token> -> ExprResult =
        function
        | ({ Type = TokenType.Bang } as operator) :: rest ->
            let expr, rest = primary rest
            Unary(operator, Bang, expr), rest
        | ({ Type = TokenType.Minus } as operator) :: rest ->
            let expr, rest = primary rest
            Unary(operator, Minus, expr), rest
        | token -> primary token

    and binary nextPrec opTokens tokens : ExprResult =
        let rec go expr =
            function
            | operator :: rest when List.contains operator.Type opTokens ->
                match BinaryOp.ofToken operator with
                | Some op ->
                    let right, rest = nextPrec rest
                    go (Binary(operator, expr, op, right)) rest
                | None -> expr, operator :: rest
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
    let rec go (tokens: list<Token>) =
        match tokens with
        | [ { Type = Eof } ] -> []
        | _ ->
            let stmt, rest = Grammar.statement tokens
            stmt :: go rest

    try
        tokens |> List.ofSeq |> go |> Some
    with
    | ParseError -> None
