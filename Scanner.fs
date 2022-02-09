module Scanner

open System
open Error
open Extensions
open Token

type private Scanner(source) =
    let mutable source = source |> List.ofSeq
    let mutable line = 1
    let mutable tokens = []

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

    let addToken (token: TokenType) (lexeme: string) (line: int) =
        tokens <-
            { Type = token
              Lexeme = lexeme
              Line = line }
            :: tokens

    let scanOne () : option<char> =
        match List.splitAt 1 source with
        | ([ c ], rest) ->
            source <- rest
            line <- if c = '\n' then line + 1 else line
            Some c
        | _ -> None

    let scanWhile (f: char -> bool) : list<char> =
        let (cs, rest) = List.splitWhile f source

        source <- rest
        line <- line + List.length (List.filter ((=) '\n') cs)

        cs

    let scanIdentifier (first: char) =
        let after =
            scanWhile (fun c -> c = '_' || Char.IsLetterOrDigit c)

        let text = first :: after |> String.ofSeq

        let tokenType =
            match Map.tryFind text keywords with
            | Some t -> t
            | None -> Identifier

        addToken tokenType text line

    let consume char message =
        if scanOne () <> Some char then
            Error.Report(Some line, message)

    let scanNumber (first: char) =
        let wholePart =
            first :: scanWhile Char.IsDigit |> String.ofSeq
        // Look for a fractional part.
        let num =
            match source with
            | '.' :: c :: _ when Char.IsDigit(c) ->
                consume '.' "Failed to consume '.' in number."
                let decimalPart = scanWhile Char.IsDigit |> String.ofSeq
                wholePart + "." + decimalPart
            | _ -> wholePart

        addToken (Number(float num)) num line

    let scanString () =
        let str = scanWhile ((<>) '"') |> String.ofSeq
        consume '"' "Unterminated string."
        addToken (String str) str line

    let comment () =
        source <- List.skipWhile ((<>) '\n') source

    let scanToken () =
        match scanOne (), source with
        | Some '(', _ -> addToken LeftParen "(" line
        | Some ')', _ -> addToken RightParen ")" line
        | Some '{', _ -> addToken LeftBrace "{" line
        | Some '}', _ -> addToken RightBrace "}" line
        | Some ',', _ -> addToken Comma "," line
        | Some '.', _ -> addToken Dot "." line
        | Some '-', _ -> addToken Minus "-" line
        | Some '+', _ -> addToken Plus "+" line
        | Some ';', _ -> addToken Semicolon ";" line
        | Some '*', _ -> addToken Star "*" line
        | Some '!', '=' :: _ ->
            scanOne () |> ignore
            addToken BangEqual "!=" line
        | Some '!', _ -> addToken Bang "!" line
        | Some '=', '=' :: _ ->
            scanOne () |> ignore
            addToken EqualEqual "==" line
        | Some '=', _ -> addToken Equal "=" line
        | Some '<', '=' :: _ ->
            scanOne () |> ignore
            addToken LessEqual "<=" line
        | Some '<', _ -> addToken Less "<" line
        | Some '>', '=' :: _ ->
            scanOne () |> ignore
            addToken GreaterEqual ">=" line
        | Some '>', _ -> addToken Greater ">" line
        | Some '/', '/' :: _ -> comment ()
        | Some '/', _ -> addToken Slash "/" line

        | Some ' ', _
        | Some '\r', _
        | Some '\t', _
        | Some '\n', _ -> ()

        | Some '"', _ -> scanString ()

        | Some c, _ when Char.IsDigit(c) -> scanNumber c
        | Some c, _ when Char.IsLetter(c) || c = '_' -> scanIdentifier c

        | Some c, _ -> Error.Report(Some line, $"Unexpected character: '%c{c}'")
        | None, _ -> failwith "Unexpected end of file."

    member this.ScanTokens() : list<Token> =
        match source with
        | [] -> eof line :: tokens |> List.rev
        | _ ->
            scanToken ()
            this.ScanTokens()

let scan s = Scanner(s).ScanTokens()
