module Parser

open Error
open Token

exception ParseError

type Parser(tokens: seq<Token>) =
    let tokens = tokens |> Array.ofSeq
    let mutable current = 0

    member this.Parse() =
        try
            this.Expression() |> Some
        with
        | ParseError -> None

    member private this.Expression() = this.Equality()

    member private this.Equality() =
        let mutable expr = this.Comparison()

        while this.Match([ BangEqual; EqualEqual ]) do
            let operator = this.Previous()
            let right = this.Comparison()
            expr <- Expr.Binary(expr, operator, right)

        expr

    member private this.Comparison() =
        let mutable expr = this.Term()

        while this.Match(
            [ Greater
              GreaterEqual
              Less
              LessEqual ]
        ) do
            let operator = this.Previous()
            let right = this.Term()
            expr <- Expr.Binary(expr, operator, right)

        expr

    member private this.Term() =
        let mutable expr = this.Factor()

        while this.Match([ Minus; Plus ]) do
            let operator = this.Previous()
            let right = this.Factor()
            expr <- Expr.Binary(expr, operator, right)

        expr

    member private this.Factor() =
        let mutable expr = this.Unary()

        while this.Match([ Slash; Star ]) do
            let operator = this.Previous()
            let right = this.Unary()
            expr <- Expr.Binary(expr, operator, right)

        expr

    member private this.Unary() =
        if this.Match([ Bang; Minus ]) then
            let operator = this.Previous()
            let right = this.Unary()
            Expr.Unary(operator, right)
        else
            this.Primary()

    member private this.Primary() =
        if this.Match(False) then
            Expr.Literal(false)
        elif this.Match(True) then
            Expr.Literal(true)
        elif this.Match(Nil) then
            Expr.Literal(null)
        elif this.Match([ Number; String ]) then
            Expr.Literal(this.Previous().Literal)
        elif this.Match(LeftParen) then
            let expr = this.Expression()

            this.Consume(RightParen, "Expect ')' after expression.")
            |> ignore

            Expr.Grouping(expr)
        else
            raise
            <| this.Error(this.Peek(), "Expect expression.")

    member private this.Match(type_: TokenType) = this.Match([ type_ ])

    member private this.Match(types: seq<TokenType>) =
        seq {
            for type_ in types do
                if this.Check(type_) then
                    this.Advance() |> ignore
                    true
                else
                    false
        }
        |> Seq.exists id

    member private this.Consume(type_, message) : Token =
        if this.Check(type_) then
            this.Advance()
        else
            raise <| this.Error(this.Peek(), message)

    member private this.Check(type_) =
        if this.IsAtEnd() then
            false
        else
            this.Peek().Type = type_

    member private this.Advance() : Token =
        if not (this.IsAtEnd()) then
            current <- current + 1

        this.Previous()

    member private this.IsAtEnd() = this.Peek().Type = Eof

    member private _.Peek() : Token = tokens.[current]

    member private _.Previous() : Token = tokens.[current - 1]

    member private _.Error(token: Token, message) =
        Error.Report(token, message)
        ParseError

    member private this.Synchronize() =
        let isStatementStart () =
            match this.Peek().Type with
            | Class
            | Fun
            | Var
            | For
            | If
            | While
            | Print
            | Return -> true
            | _ -> false

        this.Advance() |> ignore

        while not (this.IsAtEnd())
              && this.Previous().Type <> Semicolon
              && not (isStatementStart ()) do
            this.Advance() |> ignore
