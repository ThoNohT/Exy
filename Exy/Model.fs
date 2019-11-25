module Model

/// The possible types in the language.
type Type = Number | Truth

/// A type that indicates whether something is true or not.
type Truth = Yes | No

module Truth =
    /// Convert a bool to a truth value.
    let fromBool b = if b then Yes else No

/// A value type that can contain all known types.
type Value =
    | Number of decimal
    | Truth of Truth

module Value =
    /// Display a value.
    let display v  =
        match v with
        | Value.Number n -> sprintf "%M" n
        | Value.Truth t -> sprintf "%A" t

    /// Get the type of a value.
    let getType (v: Value): Type =
        match v with
        | Number _ -> Type.Number
        | Truth _ -> Type.Truth

/// Comparison types, all can be applied to numbers and truths.
type Comparison = Same | Gt | Lt | Gts | Lts

module Comparison =
    /// Display a comparison symbol.
    let display comp =
        match comp with
        | Same -> "="
        | Gt -> ">"
        | Lt -> "<"
        | Gts -> ">="
        | Lts -> "<="

    /// Perform a comparison of two values given a comparison
    let compare comparison a b =
        let numComp c a b =
            Truth.fromBool <|
                match c with
                | Same -> a = b
                | Gt -> a > b
                | Lt -> a < b
                | Gts -> a >= b
                | Lts -> a <= b

        let truthComp c a b =
            Truth.fromBool <|
                match c with
                | Same -> a = b
                | Gt -> a = Yes && b = No
                | Lt -> a = No && b = Yes
                | Gts -> a = Yes || b = No
                | Lts -> b = Yes || a = No

        match (a, b) with
        | (Value.Number an, Value.Number bn) -> Ok (Value.Truth <| numComp comparison an bn)
        | (Value.Truth at, Value.Truth bt) -> Ok (Value.Truth <| truthComp comparison at bt)
        | _ -> failwith "Invalid types."

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
    /// Comparison of two expressions.
    | Com of Comparison * Expression * Expression
    /// A grouping of an expression.
    | Grp of Expression
    /// A variable.
    | Var of string
    /// A number.
    | Num of decimal
    /// A boolean value.
    | Truth of Truth

/// The state allows expressions to be bound to variables.
type State = Map<string, Expression>

module State =
    /// Lookup a variable in the state.
    let lookup (state:State) var =
        state |> Map.tryFind var

module Expression =
    /// Indicates whether an expression is binary or not.
    let isBinary expression =
        match expression with
        | Grp _ -> false
        | Var _ -> false
        | Num _ -> false
        | _ -> true

    /// Display an expression.
    let rec display expression =
        match expression with
        | Div (a, b) -> sprintf "%s / %s" (display a) (display b)
        | Mul (a, b) -> sprintf "%s * %s" (display  a) (display b)
        | Rem (a, b) -> sprintf "%s %% %s" (display  a) (display  b)
        | Add (a, b) -> sprintf "%s + %s" (display  a) (display  b)
        | Sub (a, b) -> sprintf "%s - %s" (display  a) (display  b)
        | Com (c, a, b) -> sprintf "%s %s %s" (display  a) (Comparison.display c) (display  b)
        | Grp s -> sprintf "( %s )" (display  s)
        | Var v -> v
        | Num n -> sprintf "%M" n
        | Truth t -> sprintf "%A" t

    /// Expand an expression a single step: Evaluate the expression or number behind a variable.
    let rec expandVariable state expression =
        match expression with
        | Var v -> State.lookup state v |> Option.map (expandVariable state) |> Option.defaultValue expression
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
            | Com (c, a, b) -> Com (c, (expand' state a false), (expand' state b false))
            | Grp g -> Grp (expand' state g false)
            | Num n -> Num n
            | Truth t -> Truth t
            | Var v ->
                State.lookup state v
                    |> Option.map
                        (fun e ->
                            expand' state e false
                                // If this is the root expansion, no parentheses are needed around a binary expression.
                                |> if isBinary e && (not root) then Grp else id
                        )
                    |> Option.defaultValue (Var v)

        expand' state expression true

    /// Determine the type of an expression given a certain state.
    let rec getType state expression =
        match expression with
        | Div _ -> Ok Type.Number
        | Mul _ -> Ok Type.Number
        | Rem _ -> Ok Type.Number
        | Add _ -> Ok Type.Number
        | Sub _ -> Ok Type.Number
        | Com _ -> Ok Type.Truth
        | Grp s -> getType state s
        | Var v -> State.lookup state v |> Option.map (getType state) |> Result.fromOption (sprintf "Unbound variable %s" v)
        | Num _ -> Ok Type.Number
        | Truth _ -> Ok Type.Truth

    /// Evaluate an expression given a certain state.
    let rec evaluate state expression =
        let ifNum f a b =
            match (a, b) with
            | (Number an, Number bn) -> f an bn |> Result.map Number
            | _ -> Result.fail <| sprintf "Expression %A can only be evaluated for numbers." (display expression)

        let safeDiv a b = if b = 0M then Result.fail "Division by 0." else Ok (a / b)

        let matchTypes state f (a: Expression) (b: Expression): Result<Value, Set<string>> =
            let performIfSame aEval bEval =
                if Value.getType aEval = Value.getType bEval then
                    f aEval bEval
                else
                    sprintf "Type %A of %s is not compatible with type %A of %s" (Value.getType aEval) (display a) (Value.getType bEval) (display b)
                    |> Result.fail

            Result.bind2 performIfSame (evaluate state a) (evaluate state b)

        match expression with
        | Sub (a, b) -> Result.bind2 (ifNum (Result.hoist2 (-))) (evaluate state a) (evaluate state b)
        | Add (a, b) -> Result.bind2 (ifNum (Result.hoist2 (+))) (evaluate state a) (evaluate state b)
        | Rem (a, b) -> Result.bind2 (ifNum (Result.hoist2 (%))) (evaluate state a) (evaluate state b)
        | Mul (a, b) -> Result.bind2 (ifNum (Result.hoist2 (*))) (evaluate state a) (evaluate state b)
        | Div (a, b) -> Result.bind2 (ifNum safeDiv) (evaluate state a) (evaluate state b)
        | Com (c, a, b) -> matchTypes state (Comparison.compare c) a b
        | Grp (g) -> evaluate state g
        | Num n -> Ok (Value.Number n)
        | Truth t -> Ok (Value.Truth t)
        | Var v -> State.lookup state v |> Option.map (evaluate state) |> Result.fromOption (sprintf "Unbound variable: %s." v)

    /// Return all variables used in an expression.
    let rec usedVars state expression : Set<string> =
        match expression with
        | Div (a, b) -> Set.union (usedVars state a) (usedVars state b)
        | Mul (a, b) -> Set.union (usedVars state a) (usedVars state b)
        | Rem (a, b) -> Set.union (usedVars state a) (usedVars state b)
        | Add (a, b) -> Set.union (usedVars state a) (usedVars state b)
        | Sub (a, b) -> Set.union (usedVars state a) (usedVars state b)
        | Com (_,a , b) -> Set.union (usedVars state a) (usedVars state b)
        | Grp s -> usedVars state s
        | Var v -> Set.add v ((State.lookup state v) |> Option.map (usedVars state) |> Option.defaultValue Set.empty)
        | Num _ -> Set.empty
        | Truth _ -> Set.empty

/// A statement represents an action that can be performed.
type Statement =
    /// Binding of an expression to a variable.
    | Binding of string * Expression
    /// Clearing one, or all variable bindings.
    | Clear of string option
    /// Calculation of an expression.
    | Calculation of Expression
    /// Exit the program.
    | Exit
    /// Save the current state to a file.
    | Save of string
    /// Load new state from a file.
    | Load of string