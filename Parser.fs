module Parser

open Error
open Token
open Ast

let (<<.) a _ = a

type Parser(tokens) =
    let mutable tokens = tokens

    let parseError (token: option<Token>) message =
        Error.Report(token, message)
        ParseError(tokens)

    let logError (token: option<Token>) message = parseError token message |> ignore

    let raiseError (token: option<Token>) message = raise <| parseError token message

    let eofError () =
        raiseError None "Unexpected end of file."

    let parseOne () =
        match List.splitAt 1 tokens with
        | [ token ], rest ->
            tokens <- rest
            token
        | _ -> raiseError None "Unexpected end of file"

    let skipOne () : unit =
        match List.splitAt 1 tokens with
        | [ _ ], rest -> tokens <- rest
        | _ -> raiseError None "Unexpected end of file"

    let consume type_ message : unit =
        let token = parseOne ()

        if token.Type <> type_ then
            raiseError (Some token) message

    let peek () =
        match tokens with
        | token :: _ -> token
        | _ -> raiseError None "Unexpected end of file."

    let synchronize () : unit =
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

        tokens <-
            List.skipWhile
                (fun token ->
                    token.Type <> TokenType.Eof
                    && token.Type <> TokenType.Semicolon
                    && not (isStatementStart token))
                tokens

        if peek().Type = Semicolon then
            skipOne ()

    let parse types message =
        let token = peek ()

        if List.contains token.Type types then
            parseOne ()
        else
            raiseError (Some token) message

    let tryParse types =
        let token = peek ()

        if List.contains token.Type types then
            parseOne () |> Some
        else
            None

    let rec declaration () =
        try
            match peek().Type with
            | TokenType.Var ->
                skipOne ()
                varDeclaration ()
            | TokenType.Fun ->
                skipOne ()
                Function(funDeclaration "function")
            | _ -> statement ()
            |> Some
        with
        | ParseError rest ->
            synchronize ()
            None

    and varDeclaration () : Stmt =
        let identifier =
            parse [ Identifier ] "Expect variable name."

        let value =
            match peek().Type with
            | Equal ->
                skipOne ()
                Some <| expression ()
            | _ -> None

        consume Semicolon "Expect ';' after variable declaration."
        Stmt.Var(identifier, value)

    and funDeclaration (kind: string) : LoxFunction =
        let identifier =
            parse [ Identifier ] $"Expect %s{kind} name."

        consume LeftParen $"Expect '(' after %s{kind} name."
        let ``params`` = parameters ()
        consume LeftBrace $"Expect '{{' before %s{kind} body."
        LoxFunction(identifier, ``params``, block ())

    and parameters () : list<Token> =
        let next = parseOne ()

        match next.Type with
        | RightParen -> []
        | Identifier ->
            let afterIdent = parseOne ()

            match afterIdent.Type with
            | Comma ->
                let ``params`` = parameters ()

                if List.length ``params`` >= 255 then
                    runtimeError "Can't have more than 255 parameters." (peek ()).Line

                next :: ``params``
            | RightParen -> [ next ]
            | _ -> raiseError (Some afterIdent) "Expect ')' after parameters."
        | _ -> raiseError (Some next) "Expect ')' or parameter."

    and statement () : Stmt =
        match peek().Type with
        | TokenType.For ->
            skipOne ()
            forStatement ()
        | TokenType.If ->
            skipOne ()
            ifStatement ()
        | TokenType.While ->
            skipOne ()
            whileStatement ()
        | TokenType.Return ->
            skipOne ()
            returnStatement ()
        | TokenType.Print ->
            skipOne ()
            printStatement ()
        | TokenType.LeftBrace ->
            skipOne ()
            block ()
        | _ -> expressionStatement ()

    and returnStatement () : Stmt =
        let next = peek ()

        match next.Type with
        | Semicolon ->
            skipOne ()
            Stmt.Return(next, None)
        | _ ->
            let expr = expression ()
            consume Semicolon "Expect ';' after return statement."
            Stmt.Return(next, Some expr)

    and ifStatement () : Stmt =
        consume LeftParen "Expect '(' after 'if'."
        let condition = expression ()
        consume RightParen "Expect ')' after if condition."
        let thenBranch = statement ()

        let elseBranch =
            match peek().Type with
            | Else ->
                skipOne ()
                Some(statement ())
            | _ -> None

        Stmt.If(condition, thenBranch, elseBranch)

    and whileStatement () : Stmt =
        consume LeftParen "Expect '(' after 'while'."
        let condition = expression ()
        consume RightParen "Expect ')' after while condition."
        Stmt.While(condition, statement ())

    and forStatement () : Stmt =
        consume LeftParen "Expect '(' after 'for'."

        let initializer =
            match peek().Type with
            | Semicolon -> None
            | TokenType.Var ->
                skipOne ()
                varDeclaration () |> Some
            | _ -> expressionStatement () |> Some

        let condition =
            match peek().Type with
            | Semicolon -> None
            | _ ->
                Some <| expression ()
                <<. consume Semicolon "Expect ';' after loop condition."

        let increment =
            match peek().Type with
            | RightParen -> None
            | _ ->
                Some <| expression ()
                <<. consume RightParen "Expect ')' after for clauses."

        let body = statement ()

        let body =
            match increment with
            | None -> body
            | Some inc -> Stmt.Block([ body; Stmt.Expression(inc) ])

        let body =
            match condition with
            | None -> Stmt.While(Literal(Bool(true)), body)
            | Some cond -> Stmt.While(cond, body)

        match initializer with
        | None -> body
        | Some init -> Stmt.Block([ init; body ])

    and block () : Stmt =
        let statements = ResizeArray()

        while tokens.Head.Type <> RightBrace
              && tokens.Head.Type <> Eof do
            Option.iter statements.Add (declaration ())

        Stmt.Block(List.ofSeq statements)
        <<. consume RightBrace "Expect '}' after block."

    and printStatement () : Stmt =
        Stmt.Print(expression ())
        <<. consume Semicolon "Expect ';' after value."

    and expressionStatement () : Stmt =
        Stmt.Expression(expression ())
        <<. consume Semicolon "Expect ';' after expression."

    and expression () : Expr = assignment ()

    and assignment () : Expr =
        let expr = logicOr ()
        let next = peek ()

        match next.Type with
        | Equal ->
            let value =
                skipOne ()
                assignment ()

            match expr with
            | Variable v -> Expr.Assign(v, value)
            | _ ->
                logError (Some next) "Invalid assignment target"
                expr
        | _ -> expr

    and primary () : Expr =
        let token = parseOne ()

        match token.Type with
        | False -> Literal(Bool false)
        | True -> Literal(Bool true)
        | TokenType.Nil -> Literal Nil
        | TokenType.String s -> Literal(String s)
        | TokenType.Number n -> Literal(Number n)
        | TokenType.Identifier -> Variable token
        | TokenType.LeftParen ->
            let expr = expression ()

            Grouping(expr)
            <<. consume TokenType.RightParen "Expect ')' after expression."
        | _ -> raiseError (Some token) "Expect expression."

    // TODO refactor
    and call () : Expr =
        let rec go expr =
            let next = peek ()

            match next.Type with
            | LeftParen ->
                let args =
                    skipOne ()
                    arguments ()

                go (Expr.Call(expr, next, args))
            | _ -> expr

        let expr = primary ()
        let next = peek ()

        match next.Type with
        | LeftParen ->
            let args =
                skipOne ()
                arguments ()

            go (Expr.Call(expr, next, args))
        | _ -> expr

    and arguments () : list<Expr> =
        let next = peek ()

        match next.Type with
        | RightParen ->
            skipOne ()
            []
        | _ ->
            let arg = expression ()
            let afterIdent = parseOne ()

            match afterIdent.Type with
            | Comma ->
                let args = arguments ()

                if List.length args >= 255 then
                    runtimeError "Can't have more than 255 arguments." (peek ()).Line

                arg :: args
            | RightParen -> [ arg ]
            | _ -> raiseError (Some afterIdent) "Expect ')' after arguments."

    and unary () : Expr =
        let op = peek ()

        match op.Type with
        | TokenType.Bang -> Unary((op, Bang), primary ())
        | TokenType.Minus -> Unary((op, Minus), primary ())
        | _ -> call ()

    and bin cons ofToken nextPrec opTokens : Expr =
        let rec go left =
            let operator = tryParse opTokens

            match operator with
            | Some operator ->
                match ofToken operator with
                | Some op ->
                    let right = nextPrec ()
                    go (cons (left, (operator, op), right))
                | None -> left
            | None -> left

        go <| nextPrec ()

    and binary = bin Binary BinaryOp.ofToken

    and factor () =
        binary unary [ TokenType.Slash; TokenType.Star ]

    and term () =
        binary factor [ TokenType.Minus; TokenType.Plus ]

    and comparison () =
        binary
            term
            [ TokenType.Greater
              TokenType.GreaterEqual
              TokenType.Less
              TokenType.LessEqual ]

    and equality () =
        binary
            comparison
            [ TokenType.BangEqual
              TokenType.EqualEqual ]

    and logic = bin Logical LogicalOp.ofToken

    and logicAnd () = logic equality [ TokenType.And ]

    and logicOr () = logic logicAnd [ TokenType.Or ]

    member this.Parse() : list<Stmt> =
        match tokens with
        | [ { Type = Eof } ] -> []
        | _ ->
            match declaration () with
            | Some stmt -> stmt :: this.Parse()
            | None -> this.Parse()

let parse tokens = Parser(tokens).Parse()
