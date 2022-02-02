module Parser

open Error
open Token
open Ast

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
                token.Type <> TokenType.Eof
                && token.Type <> TokenType.Semicolon
                && not (isStatementStart token))
            tokens

    match rest with
    | { Type = Semicolon } :: rest -> rest
    | _ -> rest

module private Grammar =
    type StmtResult = Stmt * list<Token>
    type ExprResult = Expr * list<Token>

    let rec declaration tokens =
        try
            match tokens with
            | { Type = TokenType.Var } :: rest -> varDeclaration rest |> fun (a, b) -> Some a, b
            | _ -> statement tokens |> fun (a, b) -> Some a, b
        with
        | ParseError -> None, synchronize tokens

    and varDeclaration tokens : StmtResult =
        match tokens with
        | ({ Type = Identifier } as token) :: { Type = Equal } :: rest ->
            let initializer, rest = expression rest
            Stmt.Var(token, Some initializer), consume rest Semicolon "Expect ';' after variable declaration"
        | ({ Type = Identifier } as token) :: rest ->
            Stmt.Var(token, None), consume rest Semicolon "Expect ';' after variable declaration"
        | token :: _ -> raise <| parseError token "Expect variable name."
        | _ ->
            raise
            <| parseError (eof 0) "Unexpected end of file."

    and statement tokens : StmtResult =
        match tokens with
        | { Type = TokenType.Print } :: rest -> printStatement rest
        | _ -> expressionStatement tokens

    and printStatement tokens : StmtResult =
        let value, rest = expression tokens
        Stmt.Print(value), consume rest Semicolon "Expect ';' after value."

    and expressionStatement tokens : StmtResult =
        let expr, rest = expression tokens
        Stmt.Expression(expr), consume rest Semicolon "Expect ';' after expression."

    and expression tokens : ExprResult = assignment tokens

    and assignment tokens : ExprResult =
        let expr, rest = equality tokens

        match rest with
        | ({ Type = Equal } as equals) :: rest ->
            let value, rest = assignment rest

            match expr with
            | Variable v -> Expr.Assign(v, value), rest
            | _ ->
                parseError equals "Invalid assignment target"
                |> ignore

                expr, rest
        | _ -> expr, rest

    and primary tokens : ExprResult =
        match tokens with
        | { Type = False } :: rest -> Literal(Bool false), rest
        | { Type = True } :: rest -> Literal(Bool true), rest
        | { Type = TokenType.Nil } :: rest -> Literal Nil, rest
        | { Type = TokenType.String s } :: rest -> Literal(String s), rest
        | { Type = TokenType.Number n } :: rest -> Literal(Number n), rest
        | ({ Type = TokenType.Identifier } as token) :: rest -> Variable token, rest
        | { Type = TokenType.LeftParen } :: rest ->
            let expr, rest = expression rest
            Grouping(expr), consume rest TokenType.RightParen "Expect ')' after expression."
        | token :: _ -> raise <| parseError token "Expect expression."
        | _ ->
            raise
            <| parseError (eof 0) "Unexpected end of file."

    and unary tokens : ExprResult =
        match tokens with
        | ({ Type = TokenType.Bang } as operator) :: rest ->
            let expr, rest = primary rest
            Unary((operator, Bang), expr), rest
        | ({ Type = TokenType.Minus } as operator) :: rest ->
            let expr, rest = primary rest
            Unary((operator, Minus), expr), rest
        | token -> primary token

    and binary nextPrec opTokens tokens : ExprResult =
        let rec go expr =
            function
            | operator :: rest when List.contains operator.Type opTokens ->
                match BinaryOp.ofToken operator with
                | Some op ->
                    let right, rest = nextPrec rest
                    go (Binary(expr, (operator, op), right)) rest
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
            let stmt, rest = Grammar.declaration tokens
            stmt :: go rest

    try
        tokens
        |> List.ofSeq
        |> go
        |> List.choose id
        |> Some
    with
    | ParseError -> None
