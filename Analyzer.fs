module Analyzer

open System.Collections.Generic

open Error
open Extensions
open Token
open Ast

[<RequireQualifiedAccess>]
type private FunctionType =
    | None
    | Function

type private Analyzer() =
    let mutable scopes: list<Dictionary<string, bool>> = []

    let mutable currentFunction = FunctionType.None

    let beginScope () = scopes <- Dictionary() :: scopes
    let endScope () = scopes <- List.tail scopes

    let analysisError (token: Token) message =
        Error.Report(Some token, message)
        ParseError

    let declare name =
        match scopes with
        | scope :: _ ->
            match scope.TryFind(name.Lexeme) with
            | Some _ ->
                analysisError name $"Already a variable with this name in this scope."
                |> ignore
            | None -> scope.Add(name.Lexeme, false)
        | _ -> ()

    let define name =
        match scopes with
        | scope :: _ ->
            match scope.TryFind(name.Lexeme) with
            | Some _ -> scope.[name.Lexeme] <- true
            | None -> scope.Add(name.Lexeme, true)
        | _ -> ()

    let peek token =
        scopes
        |> List.tryHead
        |> Option.bind (fun scope -> scope.TryFind(token.Lexeme))

    let rec analyzeExpr =
        function
        | Literal _ -> ()

        | Variable token ->
            if peek token = Some false then
                analysisError token "Can't read local variable in its own initializer."
                |> ignore

        | Assign (_, binding) -> analyzeExpr binding

        | Binary (left, _, right) ->
            analyzeExpr left
            analyzeExpr right

        | Call (callee, _, args) ->
            analyzeExpr callee

            for arg in args do
                analyzeExpr arg

        | Grouping expr -> analyzeExpr expr

        | Logical (left, _, right) ->
            analyzeExpr left
            analyzeExpr right

        | Unary (_, right) -> analyzeExpr right

    and analyzeStmt =
        function
        | If (cond, thenBranch, elseBranch) ->
            analyzeExpr cond
            analyzeStmt thenBranch
            Option.iter analyzeStmt elseBranch

        | Var (token, binding) ->
            declare token
            Option.iter analyzeExpr binding
            define token

        | Function (StmtFunction (token, _, _) as fn) ->
            declare token
            define token

            analyzeFunction fn FunctionType.Function

        | Block stmts ->
            beginScope ()

            for statement in stmts do
                analyzeStmt statement

            endScope ()

        | Expression expr -> analyzeExpr expr

        | Print expr -> analyzeExpr expr

        | Return (keyword, expr) ->
            if currentFunction = FunctionType.None then
                analysisError keyword "Can't return from top-level code."
                |> ignore

            Option.iter analyzeExpr expr

        | While (cond, body) ->
            analyzeExpr cond
            analyzeStmt body

    and analyzeFunction (StmtFunction (_, ``params``, body)) fnType =
        let enclosingFunction = currentFunction
        currentFunction <- fnType

        beginScope ()

        for param in ``params`` do
            declare param
            define param

        analyzeStmt body
        endScope ()

        currentFunction <- enclosingFunction

    member _.Analyze(ast) =
        for stmt in ast do
            analyzeStmt stmt

let analyze ast = Analyzer().Analyze(ast)
