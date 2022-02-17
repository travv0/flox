module Parser

open Error
open Token
open Ast

let (.>>) a _ = a

type private Parser(tokens) =
    let mutable tokens = tokens

    let parseError (token: option<Token>) message =
        Error.Report(token, message)
        ParseError

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

    let peek () =
        match tokens with
        | token :: _ -> token
        | _ -> raiseError None "Unexpected end of file."

    let consume ``type`` message : unit =
        let token = peek ()

        if token.Type <> ``type`` then
            raiseError (Some token) message
        else
            skipOne ()

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

        if peek().Type = TokenType.Semicolon then
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
            | TokenType.Class ->
                skipOne ()
                classDeclaration ()
            | TokenType.Fun ->
                skipOne ()
                Stmt.Function(funDeclaration "function")
            | _ -> statement ()
            |> Some
        with
        | ParseError ->
            synchronize ()
            None

    and varDeclaration () : Stmt =
        let identifier =
            parse [ TokenType.Identifier ] "Expect variable name."

        let value =
            match peek().Type with
            | TokenType.Equal ->
                skipOne ()
                Some <| expression ()
            | _ -> None

        consume TokenType.Semicolon "Expect ';' after variable declaration."
        Stmt.Var(identifier, value)

    and classDeclaration () : Stmt =
        let name =
            parse [ TokenType.Identifier ] "Expect class name."

        let superclass =
            tryParse [ TokenType.Less ]
            |> Option.map (fun _ ->
                parse [ TokenType.Identifier ] "Expect superclass name."
                |> fun sc -> sc, Expr.Variable sc)

        consume TokenType.LeftBrace "Expect '{' before class body."
        let mthds = methods ()
        consume TokenType.RightBrace "Expect '}' after class body."
        Stmt.Class(name, superclass, mthds)

    and methods () : list<StmtFunction> =
        match (peek ()).Type with
        | TokenType.RightBrace
        | TokenType.Eof -> []
        | _ -> funDeclaration "method" :: methods ()

    and funDeclaration (kind: string) : StmtFunction =
        let identifier =
            parse [ TokenType.Identifier ] $"Expect %s{kind} name."

        consume TokenType.LeftParen $"Expect '(' after %s{kind} name."
        let ``params`` = parameters ()
        consume TokenType.LeftBrace $"Expect '{{' before %s{kind} body."
        StmtFunction(identifier, ``params``, block ())

    and parameters () : list<Token> =
        let next = parseOne ()

        match next.Type with
        | TokenType.RightParen -> []
        | TokenType.Identifier ->
            let afterIdent = parseOne ()

            match afterIdent.Type with
            | TokenType.Comma ->
                let ``params`` = parameters ()

                if List.length ``params`` >= 255 then
                    parseError (Some(peek ())) "Can't have more than 255 parameters."
                    |> ignore

                next :: ``params``
            | TokenType.RightParen -> [ next ]
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
        | TokenType.Return -> returnStatement ()
        | TokenType.Print ->
            skipOne ()
            printStatement ()
        | TokenType.LeftBrace ->
            skipOne ()
            block ()
        | _ -> expressionStatement ()

    and returnStatement () : Stmt =
        let keyword = parseOne ()
        let next = peek ()

        match next.Type with
        | TokenType.Semicolon ->
            skipOne ()
            Stmt.Return(keyword, None)
        | _ ->
            let expr = expression ()
            consume TokenType.Semicolon "Expect ';' after return statement."
            Stmt.Return(keyword, Some expr)

    and ifStatement () : Stmt =
        consume TokenType.LeftParen "Expect '(' after 'if'."
        let condition = expression ()
        consume TokenType.RightParen "Expect ')' after if condition."
        let thenBranch = statement ()

        let elseBranch =
            match peek().Type with
            | TokenType.Else ->
                skipOne ()
                Some(statement ())
            | _ -> None

        Stmt.If(condition, thenBranch, elseBranch)

    and whileStatement () : Stmt =
        consume TokenType.LeftParen "Expect '(' after 'while'."
        let condition = expression ()
        consume TokenType.RightParen "Expect ')' after while condition."
        Stmt.While(condition, statement ())

    and forStatement () : Stmt =
        consume TokenType.LeftParen "Expect '(' after 'for'."

        let initializer =
            match peek().Type with
            | TokenType.Semicolon -> None
            | TokenType.Var ->
                skipOne ()
                varDeclaration () |> Some
            | _ -> expressionStatement () |> Some

        let condition =
            match peek().Type with
            | TokenType.Semicolon -> None
            | _ ->
                Some <| expression ()
                .>> consume TokenType.Semicolon "Expect ';' after loop condition."

        let increment =
            match peek().Type with
            | TokenType.RightParen -> None
            | _ ->
                Some <| expression ()
                .>> consume TokenType.RightParen "Expect ')' after for clauses."

        let body = statement ()

        let body =
            match increment with
            | None -> body
            | Some inc -> Stmt.Block([ body; Stmt.Expression(inc) ])

        let body =
            match condition with
            | None -> Stmt.While(Expr.Literal(Literal.Bool(true)), body)
            | Some cond -> Stmt.While(cond, body)

        match initializer with
        | None -> body
        | Some init -> Stmt.Block([ init; body ])

    and block () : Stmt =
        let statements = ResizeArray()

        while tokens.Head.Type <> TokenType.RightBrace
              && tokens.Head.Type <> TokenType.Eof do
            Option.iter statements.Add (declaration ())

        Stmt.Block(List.ofSeq statements)
        .>> consume TokenType.RightBrace "Expect '}' after block."

    and printStatement () : Stmt =
        Stmt.Print(expression ())
        .>> consume TokenType.Semicolon "Expect ';' after value."

    and expressionStatement () : Stmt =
        Stmt.Expression(expression ())
        .>> consume TokenType.Semicolon "Expect ';' after expression."

    and expression () : Expr = assignment ()

    and assignment () : Expr =
        let expr = logicOr ()
        let next = peek ()

        match next.Type with
        | TokenType.Equal ->
            let value =
                skipOne ()
                assignment ()

            match expr with
            | Expr.Variable v -> Expr.Assign(v, value)
            | Expr.Get (object, name) -> Expr.Set(object, name, value)
            | _ ->
                logError (Some next) "Invalid assignment target."
                expr
        | _ -> expr

    and primary () : Expr =
        let token = parseOne ()

        match token.Type with
        | TokenType.False -> Expr.Literal(Literal.Bool false)
        | TokenType.True -> Expr.Literal(Literal.Bool true)
        | TokenType.Nil -> Expr.Literal Literal.Nil
        | TokenType.String s -> Expr.Literal(Literal.String s)
        | TokenType.Number n -> Expr.Literal(Literal.Number n)
        | TokenType.Super ->
            consume TokenType.Dot "Expect '.' after 'super'."

            let method =
                parse [ TokenType.Identifier ] "Expect superclass method name."

            Expr.Super(token, method)
        | TokenType.This -> Expr.This(token)
        | TokenType.Identifier -> Expr.Variable token
        | TokenType.LeftParen ->
            let expr = expression ()

            Expr.Grouping(expr)
            .>> consume TokenType.RightParen "Expect ')' after expression."
        | _ -> raiseError (Some token) "Expect expression."

    and call () : Expr =
        let rec go expr =
            let next = peek ()

            match next.Type with
            | TokenType.LeftParen ->
                let args =
                    skipOne ()
                    arguments ()

                go (Expr.Call(expr, next, args))
            | TokenType.Dot ->
                skipOne ()

                let name =
                    parse [ TokenType.Identifier ] "Expect property name after '.'."

                go (Expr.Get(expr, name))
            | _ -> expr

        primary () |> go

    and arguments () : list<Expr> =
        let next = peek ()

        match next.Type with
        | TokenType.RightParen ->
            skipOne ()
            []
        | _ ->
            let arg = expression ()
            let afterIdent = parseOne ()

            match afterIdent.Type with
            | TokenType.Comma ->
                let args = arguments ()

                if List.length args >= 255 then
                    parseError (Some(peek ())) "Can't have more than 255 arguments."
                    |> ignore

                arg :: args
            | TokenType.RightParen -> [ arg ]
            | _ -> raiseError (Some afterIdent) "Expect ')' after arguments."

    and unary () : Expr =
        let op = peek ()

        match op.Type with
        | TokenType.Bang ->
            skipOne ()
            Expr.Unary((op, UnaryOp.Bang), unary ())
        | TokenType.Minus ->
            skipOne ()
            Expr.Unary((op, UnaryOp.Minus), unary ())
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

    and binary = bin Expr.Binary BinaryOp.ofToken

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

    and logic = bin Expr.Logical LogicalOp.ofToken

    and logicAnd () = logic equality [ TokenType.And ]

    and logicOr () = logic logicAnd [ TokenType.Or ]

    member this.Parse() : list<Stmt> =
        match tokens with
        | [ { Type = TokenType.Eof } ] -> []
        | _ ->
            match declaration () with
            | Some stmt -> stmt :: this.Parse()
            | None -> this.Parse()

let parse tokens = Parser(tokens).Parse()
