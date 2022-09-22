module Core

// ----- Binary operators ------------------------------------------------------------------------------------------- //

/// A binary operator.
type BinOp =
    | Or // 'or'
    | And // 'and'
    | Equal // '='
    | Less // '<'
    | Add // '+'

    /// Returns a string representation of the operator.
    override this.ToString() =
        match this with
        | Or -> "||"
        | And -> "&&"
        | Equal -> "="
        | Less -> "<"
        | Add -> "+"

// ----- Unary operators -------------------------------------------------------------------------------------------- //

/// A unary operator.
type UnOp =
    | Not // 'not'
    | Neg // '-'

    /// Returns a string representation of the operator.
    override this.ToString() =
        match this with
        | Not -> "!"
        | Neg -> "-"

// ----- Values ----------------------------------------------------------------------------------------------------- //

/// A value.
type Value =
    | Int of int
    | Bool of bool

    /// Returns a string representation of the value.
    override this.ToString() =
        match this with
        | Int i -> $"{i}"
        | Bool b -> $"{b}"

// ----- Expressions ------------------------------------------------------------------------------------------------ //

/// An expression.
type Expr =
    | If of Expr * Expr * Expr
    | Binary of BinOp * Expr * Expr
    | Unary of UnOp * Expr
    | Var of string
    | Value of Value

    /// Returns a string representation of the expression.
    override this.ToString() =
        match this with
        | If (cond, thn, els) -> $"if {cond} then {thn} else {els}"
        | Binary (op, lhs, rhs) -> $"({lhs} {op} {rhs})"
        | Unary (op, operand) -> $"{op}{operand}"
        | Var x -> x
        | Value v -> $"{v}"

// ----- Commands --------------------------------------------------------------------------------------------------- //

/// A command.
type Cmd =
    | Let of string * Expr
    | Expr of Expr

    /// Returns a string representation of the command.
    override this.ToString() =
        match this with
        | Let (x, value) -> $"let {x} = {value}"
        | Expr e -> $"{e}"

// ----- Interpreter functions -------------------------------------------------------------------------------------- //

exception RuntimeError of string

/// Evaluates an expression to a value in some environment mapping variables to values.
/// If an invalid expression is encountered, the function raises a runtime error.
let rec eval (expr: Expr) (env: Map<string, Value>) : Value =
    match expr with
    | If (cond, thn, els) ->
        match eval cond env with
        | Bool true -> eval thn env
        | Bool false -> eval els env
        | _ -> raise (RuntimeError "The condition of an 'if' expression must be a boolean value")
    | Binary (op, lhs, rhs) ->
        match eval lhs env, op, eval rhs env with
        | (Bool lb, Or, Bool rb) -> Bool(lb || rb)
        | (Bool lb, And, Bool rb) -> Bool(lb && rb)
        | (Int li, Equal, Int ri) -> Bool(li = ri)
        | (Bool lb, Equal, Bool rb) -> Bool(lb = rb)
        | (Int li, Less, Int ri) -> Bool(li < ri)
        | (Int li, Add, Int ri) -> Int(li + ri)
        | _ -> raise (RuntimeError "Incompatible types in binary operation")
    | Unary (op, operand) ->
        match op, eval operand env with
        | (Not, Bool b) -> Bool(not b)
        | (Neg, Int i) -> Int(-i)
        | _ -> raise (RuntimeError "Invalid type of operand")
    | Var x ->
        match Map.tryFind x env with
        | Some v -> v
        | None -> raise (RuntimeError $"Undefined variable '{x}'")
    | Value v -> v

/// Executes a command in some environment mapping variables to values.
/// Returns the new environment after executing the command.
let execute (cmd: Cmd) (env: Map<string, Value>) : Map<string, Value> =
    match cmd with
    | Let (x, expr) -> Map.add x (eval expr env) env
    | Expr e ->
        try
            printfn $"{eval e env}"
        with
        | RuntimeError e -> printfn $"Runtime error: {e}."

        env
