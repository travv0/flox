module Token

type TokenType =
    // Single-character tokens.
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star

    // One or two character tokens.
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    // Literals.
    | Identifier of string
    | String of string
    | Number of float

    // Keywords.
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While

    | Eof

type Token =
    { Type: TokenType
      Lexeme: string
      Line: int }
