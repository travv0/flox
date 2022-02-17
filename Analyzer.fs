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
    | Initializer
    | Method

[<RequireQualifiedAccess>]
type private ClassType =
    | None
    | Class
    | Subclass

type private Analyzer() =
    let mutable scopes: list<Dictionary<string, bool>> = []

    let mutable currentFunction = FunctionType.None
    let mutable currentClass = ClassType.None

    let beginScope () = scopes <- Dictionary() :: scopes
    let endScope () = scopes <- List.tail scopes

    let analysisError (token: Token) message = Error.Report(Some token, message)

    let declare name =
        match scopes with
        | scope :: _ ->
            match scope.TryFind(name.Lexeme) with
            | Some _ -> analysisError name $"Already a variable with this name in this scope."
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
        | Expr.Literal _ -> ()

        | Expr.Variable token ->
            if peek token = Some false then
                analysisError token "Can't read local variable in its own initializer."

        | Expr.This token ->
            if currentClass = ClassType.None then
                analysisError token "Can't use 'this' outside of a class."

        | Expr.Assign (_, binding) -> analyzeExpr binding

        | Expr.Binary (left, _, right) ->
            analyzeExpr left
            analyzeExpr right

        | Expr.Call (callee, _, args) ->
            analyzeExpr callee

            for arg in args do
                analyzeExpr arg

        | Expr.Grouping expr -> analyzeExpr expr

        | Expr.Logical (left, _, right) ->
            analyzeExpr left
            analyzeExpr right

        | Expr.Unary (_, right) -> analyzeExpr right

        | Expr.Get (obj, _) -> analyzeExpr obj

        | Expr.Set (obj, _, value) ->
            analyzeExpr obj
            analyzeExpr value

        | Expr.Super (token, _) ->
            if currentClass = ClassType.None then
                analysisError token "Can't use 'super' outside of a class."
            elif currentClass <> ClassType.Subclass then
                analysisError token "Can't use 'super' in a class with no superclass."

    and analyzeStmt =
        function
        | Stmt.If (cond, thenBranch, elseBranch) ->
            analyzeExpr cond
            analyzeStmt thenBranch
            Option.iter analyzeStmt elseBranch

        | Stmt.Var (token, binding) ->
            declare token
            Option.iter analyzeExpr binding
            define token

        | Stmt.Function (StmtFunction (token, _, _) as fn) ->
            declare token
            define token

            analyzeFunction fn FunctionType.Function

        | Stmt.Block stmts ->
            beginScope ()

            for statement in stmts do
                analyzeStmt statement

            endScope ()

        | Stmt.Expression expr -> analyzeExpr expr

        | Stmt.Print expr -> analyzeExpr expr

        | Stmt.Return (keyword, expr) ->
            if currentFunction = FunctionType.None then
                analysisError keyword "Can't return from top-level code."

            Option.iter
                (fun v ->
                    if currentFunction = FunctionType.Initializer then
                        analysisError keyword "Can't return a value from an initializer."

                    analyzeExpr v)
                expr

        | Stmt.While (cond, body) ->
            analyzeExpr cond
            analyzeStmt body

        | Stmt.Class (name, superclass, methods) ->
            let enclosingClass = currentClass
            currentClass <- ClassType.Class

            declare name
            define name

            Option.iter
                (fun (scName, sc) ->
                    if name.Lexeme = scName.Lexeme then
                        analysisError scName "A class can't inherit from itself."

                    currentClass <- ClassType.Subclass
                    analyzeExpr sc

                    beginScope ()
                    let scope = List.head scopes

                    match scope.TryFind("super") with
                    | Some _ -> scope.["super"] <- true
                    | None -> scope.Add("super", true))
                superclass

            beginScope ()
            let scope = List.head scopes

            match scope.TryFind("this") with
            | Some _ -> scope.["this"] <- true
            | None -> scope.Add("this", true)

            for StmtFunction (name, _, _) as method in methods do
                let declaration =
                    if name.Lexeme = "init" then
                        FunctionType.Initializer
                    else
                        FunctionType.Method

                analyzeFunction method declaration

            endScope ()

            Option.iter (fun _ -> endScope ()) superclass

            currentClass <- enclosingClass

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
