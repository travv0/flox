module Ast

open System

open Token
open System.Collections.Generic

[<Struct>]
type Literal =
    | Bool of bool: bool
    | String of str: string
    | Number of num: float
    | Function of name: string * arity: int * env: Environment * fn: (list<Literal> -> Environment -> Literal)
    | Class of classClass: Class
    | Instance of instanceClass: Class * Dictionary<string, Literal>
    | Nil

    override this.ToString() =
        match this with
        | Bool true -> "true"
        | Bool false -> "false"
        | String s -> $"\"%s{s}\""
        | Number n -> string n
        | Function (name, _, _, _) -> $"<fn %s{name}>"
        | Class (Class.Class (name, _)) -> $"<class %s{name}>"
        | Instance (Class.Class (name, _), _) -> $"<instance %s{name}>"
        | Nil -> "nil"

    member this.Display() =
        match this with
        | String s -> $"%s{s}"
        | v -> v.ToString()

and Environment = list<Map<string, ref<Literal>>>

and Class = Class of string * Environment

[<Struct>]
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
    let ofToken { Type = type_ } =
        match type_ with
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

[<Struct>]
type LogicalOp =
    | And
    | Or

module LogicalOp =
    let ofToken { Type = type_ } =
        match type_ with
        | TokenType.And -> Some And
        | TokenType.Or -> Some Or
        | _ -> None

[<Struct>]
type UnaryOp =
    | Minus
    | Bang

[<CustomEquality; NoComparison>]
type Expr =
    | Assign of Token * Expr
    | Binary of Expr * (Token * BinaryOp) * Expr
    | Call of Expr * Token * list<Expr>
    | Get of Expr * Token
    | Logical of Expr * (Token * LogicalOp) * Expr
    | Set of Expr * Token * Expr
    | Unary of (Token * UnaryOp) * Expr
    | Literal of Literal
    | Variable of Token
    | Grouping of Expr

    interface IEquatable<Expr> with
        member this.Equals(other) = obj.ReferenceEquals(this, other)

type Function = Function of Token * list<Token> * Stmt

and Stmt =
    | Expression of Expr
    | Function of Function
    | If of Expr * Stmt * option<Stmt>
    | Print of Expr
    | Return of Token * option<Expr>
    | Var of Token * option<Expr>
    | While of Expr * Stmt
    | Block of list<Stmt>
    | Class of Token * list<Function>
