module Parser

open Error
open Token
open Ast

let private parseError (token: Token) tokens message =
    Error.Report(token, message)
    ParseError(tokens)

let private consume tokens type_ message =
    match tokens with
    | token :: rest when token.Type = type_ -> rest
    | token :: rest -> raise <| parseError token rest message
    | _ ->
        raise
        <| parseError (eof 0) tokens "Unexpected end of file."

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
        | ParseError rest -> None, synchronize rest

    and varDeclaration tokens : StmtResult =
        match tokens with
        | ({ Type = Identifier } as token) :: { Type = Equal } :: rest ->
            let initializer, rest = expression rest
            Stmt.Var(token, Some initializer), consume rest Semicolon "Expect ';' after variable declaration."
        | ({ Type = Identifier } as token) :: rest ->
            Stmt.Var(token, None), consume rest Semicolon "Expect ';' after variable declaration."
        | token :: rest ->
            raise
            <| parseError token rest "Expect variable name."
        | _ ->
            raise
            <| parseError (eof 0) tokens "Unexpected end of file."

    and statement tokens : StmtResult =
        match tokens with
        | { Type = TokenType.For } :: rest -> forStatement rest
        | { Type = TokenType.If } :: rest -> ifStatement rest
        | { Type = TokenType.While } :: rest -> whileStatement rest
        | { Type = TokenType.Print } :: rest -> printStatement rest
        | { Type = TokenType.LeftBrace } :: rest -> block rest
        | _ -> expressionStatement tokens

    and ifStatement tokens : StmtResult =
        let rest =
            consume tokens LeftParen "Expect '(' after 'if'."

        let condition, rest = expression rest

        let rest =
            consume rest RightParen "Expect ')' after if condition."

        let thenBranch, rest = statement rest

        let elseBranch, rest =
            match rest with
            | { Type = Else } :: rest -> statement rest |> fun (s, r) -> Some s, r
            | _ -> None, rest

        Stmt.If(condition, thenBranch, elseBranch), rest

    and whileStatement tokens : StmtResult =
        let rest =
            consume tokens LeftParen "Expect '(' after 'while'."

        let condition, rest = expression rest

        let rest =
            consume rest RightParen "Expect ')' after while condition."

        let body, rest = statement rest
        Stmt.While(condition, body), rest

    and forStatement tokens : StmtResult =
        let rest =
            consume tokens LeftParen "Expect '(' after 'for'."

        let initializer, rest =
            match rest with
            | { Type = Semicolon } :: rest -> None, rest
            | { Type = TokenType.Var } :: rest -> varDeclaration rest |> fun (d, r) -> Some d, r
            | _ ->
                expressionStatement rest
                |> fun (e, r) -> Some e, r

        let condition, rest =
            match rest with
            | { Type = Semicolon } :: rest -> None, rest
            | _ ->
                expression rest
                |> fun (e, r) -> Some e, consume r Semicolon "Expect ';' after loop condition."

        let increment, rest =
            match rest with
            | { Type = RightParen } :: rest -> None, rest
            | _ ->
                expression rest
                |> fun (e, r) -> Some e, consume r RightParen "Expect ')' after for clauses."

        let body, rest = statement rest

        let body =
            match increment with
            | None -> body
            | Some inc -> Stmt.Block([ body; Stmt.Expression(inc) ])

        let body =
            match condition with
            | None -> Stmt.While(Literal(Bool(true)), body)
            | Some cond -> Stmt.While(cond, body)

        let body =
            match initializer with
            | None -> body
            | Some init -> Stmt.Block([ init; body ])

        body, rest

    and block tokens : StmtResult =
        let statements = ResizeArray()
        let mutable rest = tokens

        while rest.Head.Type <> RightBrace
              && rest.Head.Type <> Eof do
            let decl, r = declaration rest
            decl |> Option.iter statements.Add
            rest <- r

        Stmt.Block(statements |> List.ofSeq), consume rest RightBrace "Expect '}' after block."

    and printStatement tokens : StmtResult =
        let value, rest = expression tokens
        Stmt.Print(value), consume rest Semicolon "Expect ';' after value."

    and expressionStatement tokens : StmtResult =
        let expr, rest = expression tokens
        Stmt.Expression(expr), consume rest Semicolon "Expect ';' after expression."

    and expression tokens : ExprResult = assignment tokens

    and assignment tokens : ExprResult =
        let expr, rest = logicOr tokens

        match rest with
        | ({ Type = Equal } as equals) :: rest ->
            let value, rest = assignment rest

            match expr with
            | Variable v -> Expr.Assign(v, value), rest
            | _ ->
                parseError equals rest "Invalid assignment target"
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
        | token :: rest ->
            raise
            <| parseError token rest "Expect expression."
        | _ ->
            raise
            <| parseError (eof 0) tokens "Unexpected end of file."

    and call tokens : ExprResult =
        let mutable expr, rest = primary tokens
        let mutable callsCompleted = false

        while not callsCompleted do
            match rest with
            | ({ Type = LeftParen } as paren) :: rest' ->
                let args, rest' = arguments rest'
                expr <- Expr.Call(expr, paren, args)
                rest <- rest'
            | _ -> callsCompleted <- true

        expr, rest

    and arguments tokens : list<Expr> * list<Token> =
        match tokens with
        | { Type = RightParen } :: rest -> [], rest
        | _ ->
            let arg, rest = expression tokens

            let rec arguments' tokens =
                match tokens with
                | { Type = Comma } :: rest ->
                    let arg, rest = expression rest

                    arguments' rest
                    |> fun (args, rest) ->
                        if List.length args >= 255 then
                            parseError (List.head rest) rest "Can't have more than 255 arguments"
                            |> ignore

                        arg :: args, rest
                | _ -> [], consume tokens RightParen "Expect ')' after arguments."

            arguments' rest
            |> fun (args, rest) -> arg :: args, rest

    and unary tokens : ExprResult =
        match tokens with
        | ({ Type = TokenType.Bang } as operator) :: rest ->
            let expr, rest = primary rest
            Unary((operator, Bang), expr), rest
        | ({ Type = TokenType.Minus } as operator) :: rest ->
            let expr, rest = primary rest
            Unary((operator, Minus), expr), rest
        | _ -> call tokens

    and bin cons ofToken nextPrec opTokens tokens : ExprResult =
        let rec go expr =
            function
            | operator :: rest when List.contains operator.Type opTokens ->
                match ofToken operator with
                | Some op ->
                    let right, rest = nextPrec rest
                    go (cons (expr, (operator, op), right)) rest
                | None -> expr, operator :: rest
            | rest -> expr, rest

        let expr, rest = nextPrec tokens
        go expr rest

    and binary = bin Binary BinaryOp.ofToken

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

    and logic = bin Logical LogicalOp.ofToken

    and logicAnd = logic equality [ TokenType.And ]

    and logicOr = logic logicAnd [ TokenType.Or ]

let parse tokens =
    let rec go (tokens: list<Token>) =
        match tokens with
        | [] -> []
        | [ { Type = Eof } ] -> []
        | _ ->
            let stmt, rest = Grammar.declaration tokens
            stmt :: go rest

    tokens
    |> List.ofSeq
    |> go
    |> List.choose id
