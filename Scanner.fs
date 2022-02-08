module Scanner

open FSharpx.State
open Error
open Extensions
open Token
open System

type ScanError =
    { scanErrorLine: int
      scanErrorMessage: string }

type ScannerState =
    { Source: list<char>
      Line: int
      Tokens: list<Token>
      Errors: list<ScanError> }

type Scanner<'a> = State<'a, ScannerState>
type Scanner = Scanner<unit>

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
      Tokens = []
      Errors = [] }

let modify (f: 's -> 's) : State<unit, 's> =
    state {
        let! s = getState
        do! putState (f s)
    }

let private addToken (token: TokenType) (lexeme: string) (line: int) : Scanner =
    state {
        do!
            modify (fun s ->
                { s with
                    Tokens =
                        { Type = token
                          Lexeme = lexeme
                          Line = line }
                        :: s.Tokens })
    }

let scanOne: Scanner<option<char>> =
    state {
        let! scanner = getState

        match List.splitAt 1 scanner.Source with
        | ([ c ], rest) ->
            do!
                modify (fun s ->
                    { s with
                        Source = rest
                        Line = if c = '\n' then s.Line + 1 else s.Line })

            return Some c
        | _ -> return None
    }

let scanWhile (f: char -> bool) : Scanner<list<char>> =
    state {
        let! scanner = getState
        let (cs, rest) = List.splitWhile f scanner.Source

        do!
            modify (fun s ->
                { s with
                    Source = rest
                    Line = s.Line + List.length (List.filter ((=) '\n') cs) })

        return cs
    }

let private scanIdentifier (first: char) : Scanner =
    state {
        let! scanner = getState
        let! after = scanWhile (fun c -> c = '_' || Char.IsLetterOrDigit c)

        let ident = first :: after
        let text = String.ofSeq ident

        let tokenType =
            match Map.tryFind text keywords with
            | Some t -> t
            | None -> Identifier

        do! addToken tokenType text scanner.Line
    }

let consume char message =
    state {
        let! scanner = getState
        let! c = scanOne

        if c <> Some char then
            Error.Report(scanner.Line, message)
    }

let private scanNumber first : Scanner =
    state {
        let! scanner = getState
        let! wholePart = scanWhile Char.IsDigit

        // Look for a fractional part.
        let! num =
            state {
                match scanner.Source with
                | '.' :: c :: _ when Char.IsDigit(c) ->
                    do! consume '.' "Failed to consume '.' in number."
                    let! cs = scanWhile Char.IsDigit
                    return String.ofSeq wholePart + "." + String.ofSeq cs
                | _ -> return String.ofSeq wholePart
            }

        return! addToken (Number(float num)) num scanner.Line
    }

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

let rec scanTokens: Scanner<list<Token>> =
    function
    | { Source = []
        Tokens = tokens
        Line = line } -> eof line :: tokens |> List.rev
    | scanner -> scanToken scanner |> scanTokens
