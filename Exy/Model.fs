module Model

/// An expression represents a value, or a calculation leading to a value.
type Expression =
    /// Division.
    | Div of Expression * Expression
    /// Multiplication.
    | Mul of Expression * Expression
    /// Remainder.
    | Rem of Expression * Expression
    /// Addition.
    | Add of Expression * Expression
    /// Subtraction.
    | Sub of Expression * Expression
    /// A grouping of an expression.
    | Grp of Expression
    /// A variable.
    | Var of string
    /// A number.
    | Num of decimal

/// A statement represents an action that can be performed.
type Statement =
    /// Binding of an expression to a variable.
    | Binding of string * Expression
    /// Clearing a variable binding.
    | Clear of string
    /// Calculation of an expression.
    | Calculation of Expression
    /// Exit the program.
    | Exit
    /// Save the current state to a file.
    | Save of string
    /// Load new state from a file.
    | Load of string

/// The state allows expressions to be bound to variables.
type State = Map<string, Expression>

/// Lookup a variable in the state.
let private lookup (state:State) var =
    state |> Map.tryFind var

/// Indicates whether an expression is binary or not.
let private isBinary expression =
    match expression with
    | Grp s -> false
    | Var v -> false
    | Num n -> false
    | _ -> true

/// Display an expression.
let rec displayExpr expression =
    match expression with
    | Div (a, b) -> sprintf "%s / %s" (displayExpr a) (displayExpr b)
    | Mul (a, b) -> sprintf "%s * %s" (displayExpr a) (displayExpr b)
    | Rem (a, b) -> sprintf "%s %% %s" (displayExpr a) (displayExpr b)
    | Add (a, b) -> sprintf "%s + %s" (displayExpr a) (displayExpr b)
    | Sub (a, b) -> sprintf "%s - %s" (displayExpr a) (displayExpr b)
    | Grp s -> sprintf "( %s )" (displayExpr s)
    | Var v -> v
    | Num n -> sprintf "%M" n

/// Expand an expression a single step: Evaluate the expression or number behind a variable.
let rec expandVariable state expression =
    match expression with
    | Var v -> lookup state v |> Option.map (expandVariable state) |> Option.defaultValue expression
    | _ -> expression

/// Expand an expression as far as possible. If an unbound variable is used, it is returned in the expression.
let expand state expression =
    let rec expand' state expr root =
        match expr with
        | Sub (a, b) -> Sub ((expand' state a false), (expand' state b false))
        | Add (a, b) -> Add ((expand' state a false), (expand' state b false))
        | Rem (a, b) -> Rem ((expand' state a false), (expand' state b false))
        | Mul (a, b) -> Mul ((expand' state a false), (expand' state b false))
        | Div (a, b) -> Div ((expand' state a false), (expand' state b false))
        | Grp g -> Grp (expand' state g false)
        | Num n -> Num n
        | Var v ->
            lookup state v
                |> Option.map
                    (fun e ->
                        expand' state e false
                            // If this is the root expansion, no parentheses are needed around a binary expression.
                            |> if isBinary e && (not root) then Grp else id
                    )
                |> Option.defaultValue (Var v)

    expand' state expression true

/// Evaluate an expression given a certain state.
let rec evaluate state expression =
    let safeDiv a b = if b = 0M then Result.fail "Division by 0" else Ok (a / b)

    match expression with
    | Sub (a, b) -> Result.map2 (-) (evaluate state a) (evaluate state b)
    | Add (a, b) -> Result.map2 (+) (evaluate state a) (evaluate state b)
    | Rem (a, b) -> Result.map2 (%) (evaluate state a) (evaluate state b)
    | Mul (a, b) -> Result.map2 (*) (evaluate state a) (evaluate state b)
    | Div (a, b) -> Result.bind2 safeDiv (evaluate state a) (evaluate state b)
    | Grp g -> evaluate state g
    | Num n -> Ok n
    | Var v -> lookup state v |> Option.map (evaluate state) |> Result.fromOption (sprintf "Unknown variable: %s" v) |> Result.unpack

/// Return all variables used in an expression.
let rec usedVars state expression : Set<string> =
    match expression with
    | Div (a, b) -> Set.union (usedVars state a) (usedVars state b)
    | Mul (a, b) -> Set.union (usedVars state a) (usedVars state b)
    | Rem (a, b) -> Set.union (usedVars state a) (usedVars state b)
    | Add (a, b) -> Set.union (usedVars state a) (usedVars state b)
    | Sub (a, b) -> Set.union (usedVars state a) (usedVars state b)
    | Grp s -> usedVars state s
    | Var v -> Set.add v ((lookup state v) |> Option.map (usedVars state) |> Option.defaultValue Set.empty)
    | Num _ -> Set.empty