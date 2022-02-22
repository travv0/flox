module Ast

open System.Collections.Generic

open Common
open Token

[<RequireQualifiedAccess>]
type FunctionType =
    | Function
    | Method
    | Initializer

[<Struct; NoComparison; NoEquality; RequireQualifiedAccess>]
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
        | Function (LoxFunction (name, _, _, _, _)) -> $"<fn %s{name}>"
        | Class (LoxClass (name, _, _, _)) -> $"<class %s{name}>"
        | Instance (LoxClass (name, _, _, _), _) -> $"<instance %s{name}>"
        | Nil -> "nil"

    member this.Display() =
        match this with
        | String s -> $"%s{s}"
        | v -> v.ToString()

and Environment = list<Map<string, ref<Literal>>>

and [<NoComparison; NoEquality>] LoxClass =
    | LoxClass of string * option<LoxClass> * Dictionary<string, LoxFunction> * Environment

    member this.FindMethod(name) =
        let (LoxClass (_, superclass, methods, _)) = this

        match methods.TryFind(name) with
        | Some fn -> Some fn
        | None ->
            superclass
            |> Option.bind (fun sc -> sc.FindMethod(name))

and [<Struct; NoComparison; NoEquality>] LoxFunction =
    | LoxFunction of
        name: string *
        arity: int *
        fnType: FunctionType *
        env: Environment *
        fn: (list<Literal> -> Environment -> int -> Literal)

[<Struct; NoComparison; RequireQualifiedAccess>]
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
        | TokenType.BangEqual -> Some BinaryOp.BangEqual
        | TokenType.EqualEqual -> Some BinaryOp.EqualEqual
        | TokenType.Greater -> Some BinaryOp.Greater
        | TokenType.GreaterEqual -> Some BinaryOp.GreaterEqual
        | TokenType.Less -> Some BinaryOp.Less
        | TokenType.LessEqual -> Some BinaryOp.LessEqual
        | TokenType.Minus -> Some BinaryOp.Minus
        | TokenType.Plus -> Some BinaryOp.Plus
        | TokenType.Slash -> Some BinaryOp.Slash
        | TokenType.Star -> Some BinaryOp.Star
        | _ -> None

[<Struct; NoComparison; RequireQualifiedAccess>]
type LogicalOp =
    | And
    | Or

module LogicalOp =
    let ofToken { Type = ``type`` } =
        match ``type`` with
        | TokenType.And -> Some LogicalOp.And
        | TokenType.Or -> Some LogicalOp.Or
        | _ -> None

[<Struct; RequireQualifiedAccess>]
type UnaryOp =
    | Minus
    | Bang

[<NoComparison; NoEquality; RequireQualifiedAccess>]
type Expr =
    | Assign of Token * Expr
    | Binary of Expr * (Token * BinaryOp) * Expr
    | Call of Expr * Token * list<Expr>
    | Get of Expr * Token
    | Logical of Expr * (Token * LogicalOp) * Expr
    | Set of Expr * Token * Expr
    | Super of Token * Token
    | This of Token
    | Unary of (Token * UnaryOp) * Expr
    | Literal of Literal
    | Variable of Token
    | Grouping of Expr

[<Struct; NoEquality; NoComparison>]
type StmtFunction = StmtFunction of Token * list<Token> * Stmt

and [<NoComparison; NoEquality; RequireQualifiedAccess>] Stmt =
    | Expression of Expr
    | Function of StmtFunction
    | If of Expr * Stmt * option<Stmt>
    | Print of Expr
    | Return of Token * option<Expr>
    | Var of Token * option<Expr>
    | While of Expr * Stmt
    | Block of list<Stmt>
    | Class of Token * option<Token * Expr> * list<StmtFunction>
