module Scanner

open System

open Error
open Extensions
open Token

type Scanner =
    { Source: list<char>
      Line: int
      Tokens: list<Token> }

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

let make source =
    { Source = source |> List.ofSeq
      Line = 1
      Tokens = [] }

let private addToken ({ Line = line; Source = source } as scanner) token literal newSource tokenLength =
    let text =
        List.take tokenLength source |> String.ofSeq

    { scanner with
        Source = newSource
        Tokens =
            { Type = token
              Lexeme = text
              Literal = Option.toObj literal
              Line = line }
            :: scanner.Tokens }

let private scanIdentifier ({ Source = source } as scanner) =
    // Get the first character of the identifier.
    let ident, rest = List.splitAt 1 source

    let ident, rest =
        List.splitWhile (fun c -> c = '_' || Char.IsLetterOrDigit(c)) rest
        |> (fun (i, r) -> (ident @ i, r))

    let text = String.ofSeq ident

    let tokenType =
        match Map.tryFind text keywords with
        | Some t -> t
        | None -> TokenType.Identifier

    addToken scanner tokenType (Some text) rest (List.length ident)

let private scanNumber ({ Source = source } as scanner) =
    let num, rest = List.splitWhile Char.IsDigit source

    // Look for a fractional part.
    let num, rest =
        match rest with
        | '.' :: c :: rest when Char.IsDigit(c) ->
            let cs, rest = List.splitWhile Char.IsDigit rest
            num @ '.' :: c :: cs, rest
        | _ -> num, rest

    addToken scanner TokenType.Number (num |> String.ofSeq |> float |> box |> Some) rest (List.length num)

let private scanString ({ Source = source; Line = line } as scanner) =
    let str, rest =
        source |> List.tail |> List.splitWhile ((<>) '"')

    // Update the line number if the string had newlines.
    let line =
        line
        + (str |> Seq.filter ((=) '\n') |> Seq.length)

    match rest with
    | [] ->
        Error.Report(line, "Unterminated string.")

        { scanner with
            Line = line
            Source = [] }
    | _ ->
        let scanner =
            addToken scanner TokenType.String (Some(String.ofSeq str)) rest (List.length str + 2)

        { scanner with
            Line = line
            Source = List.tail scanner.Source }

let private scanToken =
    function
    | { Source = source; Line = line } as scanner ->
        match source with
        | '(' :: source -> addToken scanner TokenType.LeftParen None source 1
        | ')' :: source -> addToken scanner TokenType.RightParen None source 1
        | '{' :: source -> addToken scanner TokenType.LeftBrace None source 1
        | '}' :: source -> addToken scanner TokenType.RightBrace None source 1
        | ',' :: source -> addToken scanner TokenType.Comma None source 1
        | '.' :: source -> addToken scanner TokenType.Dot None source 1
        | '-' :: source -> addToken scanner TokenType.Minus None source 1
        | '+' :: source -> addToken scanner TokenType.Plus None source 1
        | ';' :: source -> addToken scanner TokenType.Semicolon None source 1
        | '*' :: source -> addToken scanner TokenType.Star None source 1
        | '!' :: '=' :: source -> addToken scanner TokenType.BangEqual None source 2
        | '!' :: source -> addToken scanner TokenType.Bang None source 1
        | '=' :: '=' :: source -> addToken scanner TokenType.EqualEqual None source 2
        | '=' :: source -> addToken scanner TokenType.Equal None source 1
        | '<' :: '=' :: source -> addToken scanner TokenType.LessEqual None source 2
        | '<' :: source -> addToken scanner TokenType.Less None source 1
        | '>' :: '=' :: source -> addToken scanner TokenType.GreaterEqual None source 2
        | '>' :: source -> addToken scanner TokenType.Greater None source 1
        | '/' :: '/' :: source ->
            // A comment goes until the end of the Line.
            { scanner with Source = source |> List.skipWhile ((<>) '\n') }
        | '/' :: source -> addToken scanner TokenType.Slash None source 1

        | ' ' :: source
        | '\r' :: source
        | '\t' :: source ->
            // Ignore whitespace.
            { scanner with Source = source }

        | '\n' :: source ->
            { scanner with
                Line = scanner.Line + 1
                Source = source }

        | '"' :: _ -> scanString scanner

        | c :: _ when Char.IsDigit(c) -> scanNumber scanner
        | c :: _ when Char.IsLetter(c) || c = '_' -> scanIdentifier scanner

        | c :: source ->
            Error.Report(line, $"Unexpected character: '%c{c}'")
            { scanner with Source = source }

        | _ -> failwith "scanToken: no more tokens"

let rec scanTokens =
    function
    | { Source = []
        Tokens = tokens
        Line = line } -> eof line :: tokens |> List.rev
    | scanner -> scanToken scanner |> scanTokens
