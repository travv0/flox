module Interpreter

open System

open Token
open Ast
open Error
open System.Collections.Generic

type private Environment = Environment.Environment

exception Return of Token * Literal

type Interpreter(env) =
    let env: Environment = env

    do env.DefineGlobal("clock", Globals.clock)

    let equal left right =
        match left, right with
        | Number left, Number right when Double.IsNaN(left) && Double.IsNaN(right) -> true
        | Number left, Number right -> left = right
        | Bool left, Bool right -> left = right
        | String left, String right -> left = right
        | Nil, Nil -> true
        | _, _ -> false

    let notEqual left right =
        match left, right with
        | Number left, Number right -> left <> right
        | Bool left, Bool right -> left <> right
        | String left, String right -> left <> right
        | Nil, Nil -> false
        | _, _ -> true

    let isTruthy =
        function
        | Nil -> false
        | Bool v -> v
        | _ -> true

    let typeError expected value line =
        raise
        <| RuntimeError(Some value, $"Expect %s{expected}.", line)

    let getMethod obj name line : option<LoxFunction> =
        match obj with
        | Instance (LoxClass (_, superclass, methods, _), props) ->
            match methods.TryGetValue(name) with
            | true, LoxFunction (fnName, arity, ``type``, env, fn) ->
                let env = Environment(Map.empty :: env)

                env.Define(
                    { Lexeme = "this"
                      Type = TokenType.This
                      Line = line },
                    obj
                )

                LoxFunction(fnName, arity, ``type``, env.Get(), fn)
                |> Some
            | false, _ ->
                match superclass with
                | Some superclass -> 
                | None -> None
        | _ -> runtimeError "Only instances have methods." line

    let getProperty obj name =
        match obj with
        | Instance (LoxClass (_, methods, _), props) ->
            match props.TryGetValue(name.Lexeme) with
            | true, v -> v
            | false, _ ->
                getMethod obj name.Lexeme name.Line
                |> Option.map Literal.Function
                |> Option.defaultWith (fun () -> runtimeError $"No method '%s{name.Lexeme}' on object." name.Line)
        | _ -> runtimeError "Only instances have properties." name.Line

    let rec call token literal args =
        match literal with
        | Literal.Function (LoxFunction (_, arity, ``type``, env, fn)) when List.length args = arity ->
            match ``type`` with
            | Initializer ->
                fn args env |> ignore
                Environment(env).Get("this", token.Line)
            | _ -> fn args env
        | Literal.Function (LoxFunction (_, arity, _, _, _)) ->
            runtimeError $"Expected %d{arity} arguments but got %d{List.length args}." token.Line
        | Literal.Class ``class`` ->
            let instance =
                Literal.Instance(``class``, Dictionary())

            match getMethod instance "init" token.Line with
            | Some (LoxFunction (_, _, _, env, initializer)) -> initializer args env
            | None -> Nil
            |> ignore

            instance
        | _ -> typeError "function" literal token.Line

    let rec evaluate =
        function
        | Literal v -> v

        | This token -> env.Get(token)

        | Variable token -> env.Get(token)

        | Assign (token, expr) -> evaluate expr |> (fun v -> env.Assign(token, v))

        | Call (callee, token, argExprs) ->
            let fn = evaluate callee
            let args = List.map evaluate argExprs
            call token fn args

        | Grouping expr -> evaluate expr

        | Unary (({ Line = line }, Minus), expr) ->
            match evaluate expr with
            | Number n -> Number -n
            | v -> typeError "number" v line

        | Unary ((_, Bang), expr) ->
            let right = evaluate expr
            Bool(not (isTruthy right))

        | Binary (left, ({ Line = line }, op), right) ->
            match evaluate left, op, evaluate right with
            | Number left, BinaryOp.Minus, Number right -> Number(left - right)
            | Number left, Slash, Number right -> Number(left / right)
            | Number left, Star, Number right -> Number(left * right)

            | Number _, BinaryOp.Minus, badRight
            | Number _, Slash, badRight
            | Number _, Star, badRight -> typeError "number" badRight line

            | badLeft, BinaryOp.Minus, _
            | badLeft, Slash, _
            | badLeft, Star, _ -> typeError "number" badLeft line

            | Number left, Plus, Number right -> Number(left + right)
            | String left, Plus, String right -> String(left + right)

            | Number _, Plus, badRight -> typeError "number" badRight line
            | String _, Plus, badRight -> typeError "string" badRight line
            | badLeft, Plus, _ -> typeError "number or string" badLeft line

            | Number left, Greater, Number right -> Bool(left > right)
            | Number left, GreaterEqual, Number right -> Bool(left >= right)
            | Number left, Less, Number right -> Bool(left < right)
            | Number left, LessEqual, Number right -> Bool(left <= right)

            | Number _, Greater, badRight
            | Number _, GreaterEqual, badRight
            | Number _, Less, badRight
            | Number _, LessEqual, badRight -> typeError "number" badRight line

            | badLeft, Greater, _
            | badLeft, GreaterEqual, _
            | badLeft, Less, _
            | badLeft, LessEqual, _ -> typeError "number" badLeft line

            | left, BangEqual, right -> Bool(notEqual left right)
            | left, EqualEqual, right -> Bool(equal left right)

        | Logical (leftExpr, ({ Line = line }, And), rightExpr) ->
            let left = evaluate leftExpr

            if isTruthy left then
                evaluate rightExpr
            else
                left

        | Logical (leftExpr, ({ Line = line }, Or), rightExpr) ->
            let left = evaluate leftExpr

            if isTruthy left then
                left
            else
                evaluate rightExpr

        | Get (expr, name) -> getProperty (evaluate expr) name
        | Set (expr, name, valueExpr) ->
            match evaluate expr with
            | Instance (_, props) ->
                let value = evaluate valueExpr

                if props.ContainsKey(name.Lexeme) then
                    props.[name.Lexeme] <- value
                else
                    props.Add(name.Lexeme, value)

                value
            | _ -> runtimeError "Only instances have fields." name.Line

    let fnWrap ``params`` body (args: list<Literal>) (env: Ast.Environment) =
        let newEnv = Environment(Map.empty :: env)
        List.iter2 (fun p a -> newEnv.Define(p, a)) ``params`` args

        try
            Interpreter(newEnv).Execute(body)
            Nil
        with
        | Return (_, value) -> value

    member private this.Execute =
        function
        | If (cond, thenBranch, elseBranch) ->
            if isTruthy (evaluate cond) then
                this.Execute(thenBranch)
            else
                elseBranch |> Option.iter this.Execute
        | Expression expr -> evaluate expr |> ignore
        | Print expr ->
            evaluate expr
            |> fun v -> v.Display() |> printfn "%s"
        | Stmt.Return (token, expr) ->
            let value =
                expr
                |> Option.map evaluate
                |> Option.defaultValue Nil

            raise <| Return(token, value)
        | While (cond, body) ->
            while isTruthy (evaluate cond) do
                this.Execute(body)
        | Var (token, binding) ->
            binding
            |> Option.map evaluate
            |> Option.defaultValue Nil
            |> (fun v -> env.Define(token, v))
        | Function (StmtFunction (token, ``params``, body)) ->
            env.Define(token, Nil)

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
        | Block statements ->
            env.Push()

            for statement in statements do
                this.Execute(statement)

            env.Pop()
        | Class (token, superclass, classMethods) ->
            let go superclass =
                env.Define(token, Nil)

                let methods = Dictionary<string, LoxFunction>()

                for StmtFunction (token, ``params``, body) in classMethods do
                    let method: LoxFunction =
                        LoxFunction(
                            token.Lexeme,
                            List.length ``params``,
                            (if token.Lexeme = "init" then
                                 Initializer
                             else
                                 Method),
                            env.Get(),
                            fnWrap ``params`` body
                        )

                    methods.TryAdd(token.Lexeme, method) |> ignore

                env.Assign(token, Literal.Class(LoxClass(token.Lexeme, superclass, methods, env.Get())))
                |> ignore

            match superclass with
            | Some expr ->
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
        | Return (token, _) -> RuntimeError.Report("Can't return from top-level code.", token.Line)
        | RuntimeError (value, expected, line) ->
            match value with
            | Some value -> RuntimeError.Report(value, expected, line)
            | None -> RuntimeError.Report(expected, line)
