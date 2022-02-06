module Resolver

open Token
open Ast
open Error

open System.Collections.Generic

let scopes = Stack()

let beginScope () = scopes.Push(Dictionary<string, bool>())
let endScope () = scopes.Pop() |> ignore

let declare (name: Token) =
    match scopes.TryPeek() with
    | true, scope -> scope.Add(name.Lexeme, false)
    | false, _ -> ()

let define (name: Token) =
    match scopes.TryPeek() with
    | true, scope -> scope.[name.Lexeme] <- true
    | false, _ -> ()

let resolveLocal expr name =
    let mutable i, found = scopes.Count - 1, false

    while not found && i >= 0 do
        if scopes.ToArray().[i].ContainsKey(name.Lexeme) then
            Interpreter.resolve expr (scopes.Count - 1 - i)
            found <- true

        i <- i - 1

let rec resolveStmt =
    function
    | Block statements ->
        beginScope ()

        for statement in statements do
            resolveStmt statement

        endScope ()
    | Var (name, initializer) ->
        declare name

        match initializer with
        | Some init -> resolveExpr init
        | None -> ()

        define name
    | Function (name, _, _) as stmt ->
        declare name
        define name
        resolveFunction stmt
    | Expression expr -> resolveExpr expr
    | If (cond, thenBranch, elseBranch) ->
        resolveExpr cond
        resolveStmt thenBranch
        Option.iter resolveStmt elseBranch
    | Print expr -> resolveExpr expr
    | Return (_, (Some expr)) -> resolveExpr expr
    | Return (_, None) -> ()
    | While (cond, body) ->
        resolveExpr cond
        resolveStmt body

and resolveExpr =
    function
    | Variable token as expr ->
        match scopes.TryPeek() with
        | true, scope ->
            match scope.TryGetValue(token.Lexeme) with
            | true, false -> Error.Report(token, "Can't read local variable in its own initializer.")
            | _ -> ()
        | false, _ -> ()

        resolveLocal expr token
    | Assign (name, value) as expr ->
        resolveExpr value
        resolveLocal expr name
    | Binary (left, _, right) ->
        resolveExpr left
        resolveExpr right
    | Call (callee, _, args) ->
        resolveExpr callee

        for arg in args do
            resolveExpr arg
    | Grouping expr -> resolveExpr expr
    | Literal _ -> ()
    | Logical (left, _, right) ->
        resolveExpr left
        resolveExpr right
    | Unary (_, right) -> resolveExpr right

and resolveFunction =
    function
    | Function (_, parameters, body) ->
        beginScope ()

        for param in parameters do
            declare param
            define param

        resolveStmt body
        endScope ()
    | _ -> failwith "unreachable"

let resolve statements =
    for statement in statements do
        resolveStmt statement
