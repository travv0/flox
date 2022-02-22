module Scanner

open Common
open Error
open System
open Token

type private Scanner(source) =
    let mutable source = source |> List.ofSeq
    let mutable line = 1
    let mutable tokens = []

    let keywords =
        Map.ofList [ "and", TokenType.And
                     "class", TokenType.Class
                     "else", TokenType.Else
                     "false", TokenType.False
                     "fun", TokenType.Fun
                     "for", TokenType.For
                     "if", TokenType.If
                     "nil", TokenType.Nil
                     "or", TokenType.Or
                     "print", TokenType.Print
                     "return", TokenType.Return
                     "super", TokenType.Super
                     "this", TokenType.This
                     "true", TokenType.True
                     "var", TokenType.Var
                     "while", TokenType.While ]

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

    let skipOne () : unit = scanOne () |> ignore<option<char>>

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
            | None -> TokenType.Identifier

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

        addToken (TokenType.Number(float num)) num line

    let scanString () =
        let str = scanWhile ((<>) '"') |> String.ofSeq
        consume '"' "Unterminated string."
        addToken (TokenType.String str) str line

    let comment () =
        source <- List.skipWhile ((<>) '\n') source

    let scanToken () =
        match scanOne (), source with
        | Some '(', _ -> addToken TokenType.LeftParen "(" line
        | Some ')', _ -> addToken TokenType.RightParen ")" line
        | Some '{', _ -> addToken TokenType.LeftBrace "{" line
        | Some '}', _ -> addToken TokenType.RightBrace "}" line
        | Some ',', _ -> addToken TokenType.Comma "," line
        | Some '.', _ -> addToken TokenType.Dot "." line
        | Some '-', _ -> addToken TokenType.Minus "-" line
        | Some '+', _ -> addToken TokenType.Plus "+" line
        | Some ';', _ -> addToken TokenType.Semicolon ";" line
        | Some '*', _ -> addToken TokenType.Star "*" line
        | Some '!', '=' :: _ ->
            skipOne ()
            addToken TokenType.BangEqual "!=" line
        | Some '!', _ -> addToken TokenType.Bang "!" line
        | Some '=', '=' :: _ ->
            skipOne ()
            addToken TokenType.EqualEqual "==" line
        | Some '=', _ -> addToken TokenType.Equal "=" line
        | Some '<', '=' :: _ ->
            skipOne ()
            addToken TokenType.LessEqual "<=" line
        | Some '<', _ -> addToken TokenType.Less "<" line
        | Some '>', '=' :: _ ->
            skipOne ()
            addToken TokenType.GreaterEqual ">=" line
        | Some '>', _ -> addToken TokenType.Greater ">" line
        | Some '/', '/' :: _ -> comment ()
        | Some '/', _ -> addToken TokenType.Slash "/" line

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
