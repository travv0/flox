module Parser

open Error
open Token
open Ast

let private parseError (token: Token) tokens message =
    Error.Report(token, message)
    ParseError(tokens)

let private eofError () =
    raise
    <| parseError (eof 0) [] "Unexpected end of file."

let private consume type_ message tokens =
    match tokens with
    | token :: rest when token.Type = type_ -> rest
    | token :: rest -> raise <| parseError token rest message
    | _ -> eofError ()

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
            | { Type = TokenType.Var } :: rest -> varDeclaration rest
            | { Type = TokenType.Fun } :: rest -> funDeclaration "function" rest
            | _ -> statement tokens
            |> fun (a, b) -> Some a, b
        with
        | ParseError rest -> None, synchronize rest

    and varDeclaration tokens : StmtResult =
        match tokens with
        | ({ Type = Identifier } as token) :: { Type = Equal } :: rest ->
            let initializer, rest = expression rest
            Stmt.Var(token, Some initializer), consume Semicolon "Expect ';' after variable declaration." rest
        | ({ Type = Identifier } as token) :: rest ->
            Stmt.Var(token, None), consume Semicolon "Expect ';' after variable declaration." rest
        | token :: rest ->
            raise
            <| parseError token rest "Expect variable name."
        | _ -> eofError ()

    and funDeclaration (kind: string) tokens : StmtResult =
        match tokens with
        | ({ Type = Identifier } as ident) :: rest ->
            let rest =
                consume LeftParen $"Expect '(' after %s{kind} name." rest

            let paramList, rest = parameters rest

            let rest =
                consume LeftBrace $"Expect '{{' before %s{kind} body." rest

            let body, rest = block rest
            Stmt.Function(ident, paramList, body), rest
        | token :: rest ->
            raise
            <| parseError token rest $"Expect %s{kind} name."
        | _ -> eofError ()

    and parameters tokens : list<Token> * list<Token> =
        match tokens with
        | { Type = RightParen } :: rest -> [], rest
        | ({ Type = Identifier } as param) :: rest ->
            let rec parameters' tokens =
                match tokens with
                | { Type = Comma } :: ({ Type = Identifier } as param) :: rest ->
                    parameters' rest
                    |> fun (parameters, rest) ->
                        if List.length parameters >= 255 then
                            parseError (List.head rest) rest "Can't have more than 255 parameters."
                            |> ignore

                        param :: parameters, rest
                | _ -> [], consume RightParen "Expect ')' after parameters." tokens

            parameters' rest
            |> fun (parameters, rest) -> param :: parameters, rest
        | token :: rest ->
            raise
            <| parseError token rest "Expect ')' or parameter."
        | _ -> eofError ()

    and statement tokens : StmtResult =
        match tokens with
        | { Type = TokenType.For } :: rest -> forStatement rest
        | { Type = TokenType.If } :: rest -> ifStatement rest
        | { Type = TokenType.While } :: rest -> whileStatement rest
        | ({ Type = TokenType.Return } as token) :: rest -> returnStatement token rest
        | { Type = TokenType.Print } :: rest -> printStatement rest
        | { Type = TokenType.LeftBrace } :: rest -> block rest
        | _ -> expressionStatement tokens

    and returnStatement keyword tokens : StmtResult =
        let value, rest =
            match tokens with
            | { Type = Semicolon } :: rest -> None, rest
            | tokens ->
                let value, rest = expression tokens
                Some value, consume Semicolon "Expect ';' after return value." rest

        Stmt.Return(keyword, value), rest

    and ifStatement tokens : StmtResult =
        let rest =
            consume LeftParen "Expect '(' after 'if'." tokens

        let condition, rest = expression rest

        let rest =
            consume RightParen "Expect ')' after if condition." rest

        let thenBranch, rest = statement rest

        let elseBranch, rest =
            match rest with
            | { Type = Else } :: rest -> statement rest |> fun (s, r) -> Some s, r
            | _ -> None, rest

        Stmt.If(condition, thenBranch, elseBranch), rest

    and whileStatement tokens : StmtResult =
        let rest =
            consume LeftParen "Expect '(' after 'while'." tokens

        let condition, rest = expression rest

        let rest =
            consume RightParen "Expect ')' after while condition." rest

        let body, rest = statement rest
        Stmt.While(condition, body), rest

    and forStatement tokens : StmtResult =
        let rest =
            consume LeftParen "Expect '(' after 'for'." tokens

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
                |> fun (e, r) -> Some e, consume Semicolon "Expect ';' after loop condition." r

        let increment, rest =
            match rest with
            | { Type = RightParen } :: rest -> None, rest
            | _ ->
                expression rest
                |> fun (e, r) -> Some e, consume RightParen "Expect ')' after for clauses." r

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

        Stmt.Block(statements |> List.ofSeq), consume RightBrace "Expect '}' after block." rest

    and printStatement tokens : StmtResult =
        let value, rest = expression tokens
        Stmt.Print(value), consume Semicolon "Expect ';' after value." rest

    and expressionStatement tokens : StmtResult =
        let expr, rest = expression tokens
        Stmt.Expression(expr), consume Semicolon "Expect ';' after expression." rest

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
            Grouping(expr), consume TokenType.RightParen "Expect ')' after expression." rest
        | token :: rest ->
            raise
            <| parseError token rest "Expect expression."
        | _ -> eofError ()

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
                            parseError (List.head rest) rest "Can't have more than 255 arguments."
                            |> ignore

                        arg :: args, rest
                | _ -> [], consume RightParen "Expect ')' after arguments." tokens

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

    tokens |> List.ofSeq |> go |> List.choose id
