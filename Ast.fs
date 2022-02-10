module Ast

open System

open Token
open System.Collections.Generic

[<Struct; NoComparison; NoEquality>]
type Literal =
    | Bool of bool: bool
    | String of str: string
    | Number of num: float
    | Function of LoxFunction
    | Class of classClass: LoxClass
    | Instance of instanceClass: LoxClass * Dictionary<string, Literal>
    | Nil

    override this.ToString() =
        match this with
        | Bool true -> "true"
        | Bool false -> "false"
        | String s -> $"\"%s{s}\""
        | Number n -> string n
        | Function (LoxFunction (name, _, _, _)) -> $"<fn %s{name}>"
        | Class (LoxClass (name, _, _)) -> $"<class %s{name}>"
        | Instance (LoxClass (name, _, _), _) -> $"<instance %s{name}>"
        | Nil -> "nil"

    member this.Display() =
        match this with
        | String s -> $"%s{s}"
        | v -> v.ToString()

and Environment = list<Map<string, ref<Literal>>>

and [<Struct; NoComparison; NoEquality>] LoxClass = LoxClass of string * Dictionary<string, LoxFunction> * Environment

and [<Struct; NoComparison; NoEquality>] LoxFunction =
    | LoxFunction of name: string * arity: int * env: Environment * fn: (list<Literal> -> Environment -> Literal)

[<Struct; NoComparison>]
type BinaryOp =
    | Plus
    | BangEqual
    | EqualEqual
    | Minus
    | Slash
    | Star
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

module BinaryOp =
    let ofToken { Type = ``type`` } =
        match ``type`` with
        | TokenType.BangEqual -> Some BangEqual
        | TokenType.EqualEqual -> Some EqualEqual
        | TokenType.Greater -> Some Greater
        | TokenType.GreaterEqual -> Some GreaterEqual
        | TokenType.Less -> Some Less
        | TokenType.LessEqual -> Some LessEqual
        | TokenType.Minus -> Some Minus
        | TokenType.Plus -> Some Plus
        | TokenType.Slash -> Some Slash
        | TokenType.Star -> Some Star
        | _ -> None

[<Struct; NoComparison>]
type LogicalOp =
    | And
    | Or

module LogicalOp =
    let ofToken { Type = ``type`` } =
        match ``type`` with
        | TokenType.And -> Some And
        | TokenType.Or -> Some Or
        | _ -> None

[<Struct>]
type UnaryOp =
    | Minus
    | Bang

[<NoComparison; NoEquality>]
type Expr =
    | Assign of Token * Expr
    | Binary of Expr * (Token * BinaryOp) * Expr
    | Call of Expr * Token * list<Expr>
    | Get of Expr * Token
    | Logical of Expr * (Token * LogicalOp) * Expr
    | Set of Expr * Token * Expr
    | This of Token
    | Unary of (Token * UnaryOp) * Expr
    | Literal of Literal
    | Variable of Token
    | Grouping of Expr

[<Struct; NoEquality; NoComparison>]
type StmtFunction = StmtFunction of Token * list<Token> * Stmt

and Stmt =
    | Expression of Expr
    | Function of StmtFunction
    | If of Expr * Stmt * option<Stmt>
    | Print of Expr
    | Return of Token * option<Expr>
    | Var of Token * option<Expr>
    | While of Expr * Stmt
    | Block of list<Stmt>
    | Class of Token * list<StmtFunction>
