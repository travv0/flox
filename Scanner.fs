module Scanner

open System
open FSharpx.State
open Error
open Extensions
open Token

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

let private scanNumber (first: char) : Scanner =
    state {
        let! scanner = getState
        let! w = scanWhile Char.IsDigit
        let wholePart = first :: w

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

        do! addToken (Number(float num)) num scanner.Line
    }

let private scanString: Scanner =
    state {
        let! scanner = getState
        let! chars = scanWhile ((<>) '"')
        let str = String.ofSeq chars
        do! consume '"' "Unterminated string."
        do! addToken (String str) str scanner.Line
    }

let comment: Scanner =
    state {
        let! scanner = getState
        do! modify (fun s -> { s with Source = List.skipWhile ((<>) '\n') scanner.Source })
    }

let private scanToken: Scanner =
    state {
        let! c = scanOne
        let! scanner = getState

        do!
            match c, scanner.Source with
            | Some '(', _ -> addToken LeftParen "(" scanner.Line
            | Some ')', _ -> addToken RightParen ")" scanner.Line
            | Some '{', _ -> addToken LeftBrace "{" scanner.Line
            | Some '}', _ -> addToken RightBrace "}" scanner.Line
            | Some ',', _ -> addToken Comma "," scanner.Line
            | Some '.', _ -> addToken Dot "." scanner.Line
            | Some '-', _ -> addToken Minus "-" scanner.Line
            | Some '+', _ -> addToken Plus "+" scanner.Line
            | Some ';', _ -> addToken Semicolon ";" scanner.Line
            | Some '*', _ -> addToken Star "*" scanner.Line
            | Some '!', '=' :: _ -> scanOne >>. addToken BangEqual "!=" scanner.Line
            | Some '!', _ -> addToken Bang "!" scanner.Line
            | Some '=', '=' :: _ -> scanOne >>. addToken EqualEqual "==" scanner.Line
            | Some '=', _ -> addToken Equal "=" scanner.Line
            | Some '<', '=' :: _ -> scanOne >>. addToken LessEqual "<=" scanner.Line
            | Some '<', _ -> addToken Less "<" scanner.Line
            | Some '>', '=' :: _ ->
                scanOne
                >>. addToken GreaterEqual ">=" scanner.Line
            | Some '>', _ -> addToken Greater ">" scanner.Line
            | Some '/', '/' :: _ -> comment
            | Some '/', _ -> addToken Slash "/" scanner.Line

            | Some ' ', _
            | Some '\r', _
            | Some '\t', _
            | Some '\n', _ -> empty

            | Some '"', _ -> scanString

            | Some c, _ when Char.IsDigit(c) -> scanNumber c
            | Some c, _ when Char.IsLetter(c) || c = '_' -> scanIdentifier c

            | Some c, _ ->
                Error.Report(scanner.Line, $"Unexpected character: '%c{c}'")
                empty
            | None, _ -> failwith "Unexpected end of file."
    }

let rec private scanTokens () : Scanner<list<Token>> =
    state {
        let! scanner = getState

        match scanner.Source with
        | [] -> return eof scanner.Line :: scanner.Tokens |> List.rev
        | _ ->
            do! scanToken
            return! scanTokens ()
    }

let scan s =
    s
    |> List.ofSeq
    |> make
    |> scanTokens ()
    |> fun (a, b) -> a, b.Errors |> List.rev
