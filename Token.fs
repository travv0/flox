module Token

open TokenType

type Literal = option<obj>

module Literal =
    let toString =
        function
        | Some l -> l.ToString()
        | None -> "null"

type Token =
    { Type: TokenType
      Lexeme: string
      Literal: Literal
      Line: int }

    override this.ToString() =
        $"%O{this.Type} %s{this.Lexeme} %O{Literal.toString this.Literal}"
