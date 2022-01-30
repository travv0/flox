module Scanner

open System

open Error
open Token

open type TokenType.TokenType

type Scanner(source: string) =
    let tokens = ResizeArray()

    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let keywords =
        Map.ofList [ "and", And
                     "class", Class
                     "else", Else
                     "false", False
                     "fun", Fun
                     "for", For
                     "if", If
                     "nil", Nil
                     "or", Or
                     "print", Print
                     "return", Return
                     "super", Super
                     "this", This
                     "true", True
                     "var", Var
                     "while", While ]

    member this.ScanTokens() =
        while not (this.IsAtEnd()) do
            // We are at the beginning of the next lexeme.
            start <- current
            this.ScanToken()

        tokens.Add(
            { Type = Eof
              Lexeme = ""
              Literal = None
              Line = line }
        )

        tokens

    member private this.ScanToken() =
        let c = this.Advance()

        match c with
        | '(' -> this.AddToken LeftParen
        | ')' -> this.AddToken RightParen
        | '{' -> this.AddToken LeftBrace
        | '}' -> this.AddToken RightBrace
        | ',' -> this.AddToken Comma
        | '.' -> this.AddToken Dot
        | '-' -> this.AddToken Minus
        | '+' -> this.AddToken Plus
        | ';' -> this.AddToken Semicolon
        | '*' -> this.AddToken Star
        | '!' ->
            this.AddToken(
                if this.Match('=') then
                    BangEqual
                else
                    Bang
            )
        | '=' ->
            this.AddToken(
                if this.Match('=') then
                    EqualEqual
                else
                    Equal
            )
        | '<' ->
            this.AddToken(
                if this.Match('=') then
                    LessEqual
                else
                    Less
            )
        | '>' ->
            this.AddToken(
                if this.Match('=') then
                    GreaterEqual
                else
                    Greater
            )
        | '/' ->
            if this.Match('/') then
                // A comment goes until the end of the line.
                while this.Peek() <> Some '\n' && not (this.IsAtEnd()) do
                    this.Advance() |> ignore
            else
                this.AddToken(Slash)

        | ' '
        | '\r'
        | '\t' ->
            // Ignore whitespace.
            ()

        | '\n' -> line <- line + 1

        | '"' -> this.String()

        | c when Char.IsDigit(c) -> this.Number()
        | c when Char.IsLetter(c) || c = '_' -> this.Identifier()

        | _ -> error line "Unexpected character."

    member private this.Identifier() =
        let isSomeLetterOrDigit =
            function
            | Some c -> Char.IsLetterOrDigit(c) || c = '_'
            | None -> false

        while isSomeLetterOrDigit (this.Peek()) do
            this.Advance() |> ignore

        let text = source.Substring(start, current - start)

        match Map.tryFind text keywords with
        | Some t -> t
        | None -> Identifier
        |> this.AddToken

    member private this.Number() =
        let isSomeDigit =
            function
            | Some c -> Char.IsDigit(c)
            | None -> false

        while isSomeDigit (this.Peek()) do
            this.Advance() |> ignore

        // Look for a fractional part.
        if
            this.Peek() = Some '.'
            && isSomeDigit (this.PeekNext())
        then
            // Consume the "."
            this.Advance() |> ignore

            while isSomeDigit (this.Peek()) do
                this.Advance() |> ignore

        this.AddToken(Number, Double.Parse(source.Substring(start, current - start)))

    member private this.String() =
        while this.Peek() <> Some '"' && not (this.IsAtEnd()) do
            if this.Peek() = Some '\n' then
                line <- line + 1

            this.Advance() |> ignore

        if this.IsAtEnd() then
            error line "Unterminated string."
        else
            // The closing ".
            this.Advance() |> ignore

            // Trim the surrounding quotes.
            let value =
                source.Substring(start + 1, current - 1 - start)

            this.AddToken(String, value)

    member private this.Match(expected) =
        if this.IsAtEnd() then
            false
        elif source.[current] <> expected then
            false
        else
            current <- current + 1
            true

    member private this.Peek() =
        if this.IsAtEnd() then
            None
        else
            Some(source.[current])

    member private _.PeekNext() =
        if current + 1 >= String.length source then
            None
        else
            Some(source.[current + 1])

    member private _.IsAtEnd() = current >= String.length source

    member private _.Advance() =
        let c = source.[current]
        current <- current + 1
        c

    member private _.AddToken(type_, ?literal: obj) =
        let text = source.Substring(start, current - start)

        tokens.Add(
            { Type = type_
              Lexeme = text
              Literal = literal
              Line = line }
        )
