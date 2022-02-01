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

let make source =
    { Source = source |> List.ofSeq
      Line = 1
      Tokens = [] }

let private addToken ({ Line = line; Source = source } as scanner) token newSource tokenLength =
    let text =
        List.take tokenLength source |> String.ofSeq

    { scanner with
        Source = newSource
        Tokens =
            { Type = token
              Lexeme = text
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
        | None -> Identifier text

    addToken scanner tokenType rest (List.length ident)

let private scanNumber ({ Source = source } as scanner) =
    let num, rest = List.splitWhile Char.IsDigit source

    // Look for a fractional part.
    let num, rest =
        match rest with
        | '.' :: c :: rest when Char.IsDigit(c) ->
            let cs, rest = List.splitWhile Char.IsDigit rest
            num @ '.' :: c :: cs, rest
        | _ -> num, rest

    addToken scanner (Number(num |> String.ofSeq |> float)) rest (List.length num)

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
            addToken scanner (String(String.ofSeq str)) rest (List.length str + 2)

        { scanner with
            Line = line
            Source = List.tail scanner.Source }

let private scanToken =
    function
    | { Source = source; Line = line } as scanner ->
        match source with
        | '(' :: source -> addToken scanner LeftParen source 1
        | ')' :: source -> addToken scanner RightParen source 1
        | '{' :: source -> addToken scanner LeftBrace source 1
        | '}' :: source -> addToken scanner RightBrace source 1
        | ',' :: source -> addToken scanner Comma source 1
        | '.' :: source -> addToken scanner Dot source 1
        | '-' :: source -> addToken scanner Minus source 1
        | '+' :: source -> addToken scanner Plus source 1
        | ';' :: source -> addToken scanner Semicolon source 1
        | '*' :: source -> addToken scanner Star source 1
        | '!' :: '=' :: source -> addToken scanner BangEqual source 2
        | '!' :: source -> addToken scanner Bang source 1
        | '=' :: '=' :: source -> addToken scanner EqualEqual source 2
        | '=' :: source -> addToken scanner Equal source 1
        | '<' :: '=' :: source -> addToken scanner LessEqual source 2
        | '<' :: source -> addToken scanner Less source 1
        | '>' :: '=' :: source -> addToken scanner GreaterEqual source 2
        | '>' :: source -> addToken scanner Greater source 1
        | '/' :: '/' :: source ->
            // A comment goes until the end of the Line.
            { scanner with Source = source |> List.skipWhile ((<>) '\n') }
        | '/' :: source -> addToken scanner Slash source 1

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
