module Interpreter

open System

open Extensions
open Token
open Ast
open Error
open System.Collections.Generic

type private Environment = Environment.Environment

exception Return of Token * Literal

type Interpreter(env) =
    let env: Environment = env

    do env.DefineGlobal("clock", Globals.clock)
    do env.DefineGlobal("listDir", Globals.listDir)

    let equal left right =
        match left, right with
        | Literal.Number left, Literal.Number right when Double.IsNaN(left) && Double.IsNaN(right) -> true
        | Literal.Number left, Literal.Number right -> left = right
        | Literal.Bool left, Literal.Bool right -> left = right
        | Literal.String left, Literal.String right -> left = right
        | Literal.Nil, Literal.Nil -> true
        | _, _ -> false

    let notEqual left right =
        match left, right with
        | Literal.Number left, Literal.Number right -> left <> right
        | Literal.Bool left, Literal.Bool right -> left <> right
        | Literal.String left, Literal.String right -> left <> right
        | Literal.Nil, Literal.Nil -> false
        | _, _ -> true

    let isTruthy =
        function
        | Literal.Nil -> false
        | Literal.Bool v -> v
        | _ -> true

    let bindThis obj line (LoxFunction (fnName, arity, ``type``, env, fn)) =
        let env = Environment(Map.empty :: env)

        env.Define(
            { Lexeme = "this"
              Type = TokenType.This
              Line = line },
            obj
        )

        LoxFunction(fnName, arity, ``type``, env.Get(), fn)

    let getMethod obj name line : option<LoxFunction> =
        match obj with
        | Literal.Instance (``class``, _) ->
            ``class``.FindMethod(name)
            |> Option.map (bindThis obj line)
        | _ -> runtimeError "Only instances have methods." line

    let getProperty obj name =
        match obj with
        | Literal.Instance (_, props) ->
            match props.TryFind(name.Lexeme) with
            | Some v -> v
            | None ->
                getMethod obj name.Lexeme name.Line
                |> Option.map Literal.Function
                |> Option.defaultWith (fun () -> runtimeError $"No method '%s{name.Lexeme}' on object." name.Line)
        | _ -> runtimeError "Only instances have properties." name.Line

    let rec call token literal args =
        match literal with
        | Literal.Function (LoxFunction (_, arity, ``type``, env, fn)) when List.length args = arity ->
            match ``type`` with
            | FunctionType.Initializer ->
                fn args env token.Line |> ignore
                Environment(env).Get("this", token.Line)
            | _ -> fn args env token.Line
        | Literal.Function (LoxFunction (_, arity, _, _, _)) ->
            runtimeError $"Expected %d{arity} arguments but got %d{List.length args}." token.Line
        | Literal.Class ``class`` ->
            let instance =
                Literal.Instance(``class``, Dictionary())

            match getMethod instance "init" token.Line with
            | Some (LoxFunction (_, _, _, env, initializer)) -> initializer args env token.Line
            | None -> Literal.Nil
            |> ignore

            instance
        | _ -> typeError "function" literal token.Line

    let rec evaluate =
        function
        | Expr.Literal v -> v

        | Expr.Super (token, method) ->
            match env.Get(token) with
            | Literal.Class superclass ->
                match superclass.FindMethod(method.Lexeme) with
                | None -> runtimeError $"Undefined property '%s{method.Lexeme}'." method.Line
                | Some fn ->
                    let obj =
                        env.Get(
                            { Lexeme = "this"
                              Type = TokenType.This
                              Line = method.Line }
                        )

                    Literal.Function(bindThis obj method.Line fn)
            | _ -> raise <| FatalError("Non-class is superclass.")

        | Expr.This token -> env.Get(token)

        | Expr.Variable token -> env.Get(token)

        | Expr.Assign (token, expr) -> evaluate expr |> (fun v -> env.Assign(token, v))

        | Expr.Call (callee, token, argExprs) ->
            let fn = evaluate callee
            let args = List.map evaluate argExprs
            call token fn args

        | Expr.Grouping expr -> evaluate expr

        | Expr.Unary (({ Line = line }, UnaryOp.Minus), expr) ->
            match evaluate expr with
            | Literal.Number n -> Literal.Number -n
            | v -> typeError "number" v line

        | Expr.Unary ((_, UnaryOp.Bang), expr) ->
            let right = evaluate expr
            Literal.Bool(not (isTruthy right))

        | Expr.Binary (left, ({ Line = line }, op), right) ->
            match evaluate left, op, evaluate right with
            | Literal.Number left, BinaryOp.Minus, Literal.Number right -> Literal.Number(left - right)
            | Literal.Number left, BinaryOp.Slash, Literal.Number right -> Literal.Number(left / right)
            | Literal.Number left, BinaryOp.Star, Literal.Number right -> Literal.Number(left * right)

            | Literal.Number _, BinaryOp.Minus, badRight
            | Literal.Number _, BinaryOp.Slash, badRight
            | Literal.Number _, BinaryOp.Star, badRight -> typeError "number" badRight line

            | badLeft, BinaryOp.Minus, _
            | badLeft, BinaryOp.Slash, _
            | badLeft, BinaryOp.Star, _ -> typeError "number" badLeft line

            | Literal.Number left, BinaryOp.Plus, Literal.Number right -> Literal.Number(left + right)
            | Literal.String left, BinaryOp.Plus, Literal.String right -> Literal.String(left + right)

            | Literal.Number _, BinaryOp.Plus, badRight -> typeError "number" badRight line
            | Literal.String _, BinaryOp.Plus, badRight -> typeError "string" badRight line
            | badLeft, BinaryOp.Plus, _ -> typeError "number or string" badLeft line

            | Literal.Number left, BinaryOp.Greater, Literal.Number right -> Literal.Bool(left > right)
            | Literal.Number left, BinaryOp.GreaterEqual, Literal.Number right -> Literal.Bool(left >= right)
            | Literal.Number left, BinaryOp.Less, Literal.Number right -> Literal.Bool(left < right)
            | Literal.Number left, BinaryOp.LessEqual, Literal.Number right -> Literal.Bool(left <= right)

            | Literal.Number _, BinaryOp.Greater, badRight
            | Literal.Number _, BinaryOp.GreaterEqual, badRight
            | Literal.Number _, BinaryOp.Less, badRight
            | Literal.Number _, BinaryOp.LessEqual, badRight -> typeError "number" badRight line

            | badLeft, BinaryOp.Greater, _
            | badLeft, BinaryOp.GreaterEqual, _
            | badLeft, BinaryOp.Less, _
            | badLeft, BinaryOp.LessEqual, _ -> typeError "number" badLeft line

            | left, BinaryOp.BangEqual, right -> Literal.Bool(notEqual left right)
            | left, BinaryOp.EqualEqual, right -> Literal.Bool(equal left right)

        | Expr.Logical (leftExpr, (_, LogicalOp.And), rightExpr) ->
            let left = evaluate leftExpr

            if isTruthy left then
                evaluate rightExpr
            else
                left

        | Expr.Logical (leftExpr, (_, LogicalOp.Or), rightExpr) ->
            let left = evaluate leftExpr

            if isTruthy left then
                left
            else
                evaluate rightExpr

        | Expr.Get (expr, name) -> getProperty (evaluate expr) name
        | Expr.Set (expr, name, valueExpr) ->
            match evaluate expr with
            | Literal.Instance (_, props) ->
                let value = evaluate valueExpr

                if props.ContainsKey(name.Lexeme) then
                    props.[name.Lexeme] <- value
                else
                    props.Add(name.Lexeme, value)

                value
            | _ -> runtimeError "Only instances have fields." name.Line

    let fnWrap ``params`` body (args: list<Literal>) (env: Ast.Environment) _line =
        let newEnv = Environment(Map.empty :: env)
        List.iter2 (fun p a -> newEnv.Define(p, a)) ``params`` args

        try
            Interpreter(newEnv).Execute(body)
            Literal.Nil
        with
        | Return (_, value) -> value

    member private this.Execute =
        function
        | Stmt.If (cond, thenBranch, elseBranch) ->
            if isTruthy (evaluate cond) then
                this.Execute(thenBranch)
            else
                elseBranch |> Option.iter this.Execute

        | Stmt.Expression expr -> evaluate expr |> ignore

        | Stmt.Print expr ->
            evaluate expr
            |> fun v -> v.Display() |> printfn "%s"

        | Stmt.Return (token, expr) ->
            let value =
                expr
                |> Option.map evaluate
                |> Option.defaultValue Literal.Nil

            raise <| Return(token, value)

        | Stmt.While (cond, body) ->
            while isTruthy (evaluate cond) do
                this.Execute(body)

        | Stmt.Var (token, binding) ->
            binding
            |> Option.map evaluate
            |> Option.defaultValue Literal.Nil
            |> (fun v -> env.Define(token, v))

        | Stmt.Function (StmtFunction (token, ``params``, body)) ->
            env.Define(token, Literal.Nil)

            env.Assign(
                token,
                Literal.Function(
                    LoxFunction(
                        token.Lexeme,
                        List.length ``params``,
                        FunctionType.Function,
                        env.Get(),
                        fnWrap ``params`` body
                    )
                )
            )
            |> ignore

        | Stmt.Block statements ->
            env.Push()

            for statement in statements do
                this.Execute(statement)

            env.Pop()

        | Stmt.Class (token, superclass, classMethods) ->
            let go (superclass: option<LoxClass>) =
                env.Define(token, Literal.Nil)

                let env =
                    superclass
                    |> Option.map (fun sc ->
                        let env = Environment(env.Get())

                        env.Define(
                            { Lexeme = "super"
                              Type = TokenType.Super
                              Line = token.Line },
                            Literal.Class(sc)
                        )

                        env)
                    |> Option.defaultValue env

                let methods = Dictionary<string, LoxFunction>()

                for StmtFunction (token, ``params``, body) in classMethods do
                    let method: LoxFunction =
                        LoxFunction(
                            token.Lexeme,
                            List.length ``params``,
                            (if token.Lexeme = "init" then
                                 FunctionType.Initializer
                             else
                                 FunctionType.Method),
                            env.Get(),
                            fnWrap ``params`` body
                        )

                    methods.TryAdd(token.Lexeme, method) |> ignore

                env.Assign(token, Literal.Class(LoxClass(token.Lexeme, superclass, methods, env.Get())))
                |> ignore

            match superclass with
            | Some (token, expr) ->
                let superclass = evaluate expr

                match superclass with
                | Literal.Class ``class`` -> go (Some ``class``)
                | _ -> runtimeError "Superclass must be a class." token.Line
            | None -> go None

    new() = Interpreter(Environment())

    member this.Interpret(statements) =
        try
            for statement in statements do
                this.Execute(statement)
        with
        | RuntimeError (value, expected, line) ->
            match value with
            | Some value -> RuntimeError.Report(value, expected, line)
            | None -> RuntimeError.Report(expected, line)
