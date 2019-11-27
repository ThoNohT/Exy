module Model

/// The possible types in the language.
type Type = Number | Truth

/// A type that indicates whether something is true or not.
type Truth = Yes | No

module Truth =
    /// Convert a bool to a truth value.
    let fromBool b = if b then Yes else No

    /// Convert a truth value to a bool.
    let toBool t = match t with | Yes -> true | No -> false

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
    let getType v =
        match v with
        | Value.Number _ -> Type.Number
        | Value.Truth _ -> Type.Truth

    /// Get the truth of a value. Assumes type is correct.
    let getTruth v =
        match v with
        | Value.Number _ -> failwith "Cannot get truth of number."
        | Value.Truth t -> t

    /// Get the number of a value. Assumes the type is correct.
    let getNumber v =
        match v with
        | Value.Number n -> n
        | Value.Truth _ -> failwith "Cannot get number of truth."


/// A part of a variable mask
type MaskPart =
    /// A part that must match literally.
    | Literal of string
    /// A wildcard that can be any character or number.
    | Wildcard

module MaskPart =
    // Display a mask part.
    let display p =
        match p with
        | Literal s -> s
        | Wildcard -> "*"

/// A mask that can match multiple variables, which can then be aggregated over.
type VariableMask = VariableMask of MaskPart list

module VariableMask =
    /// Display a variable mask.
    let display (VariableMask m) =
        List.map MaskPart.display m |> String.concat ""

    /// Determine whether a variable matches a variable mask.
    let matches (VariableMask m) var =
        let getRegexString p =
            match p with
            | Literal s -> s
            | Wildcard -> "[A-Za-z0-9_]+"

        let regexContents = List.map getRegexString m |> String.concat ""
        let regexString = sprintf "\\A%s\\z" regexContents

        match var with
        | Regex regexString _ -> true
        | _ -> false

    /// Get all variables that match the specified mask.
    let getVariables state mask =
        let vars = Map.filter (fun k _ -> matches mask k) state
        vars


/// Aggregations can be done over groups of variables. They have two properties: Truth or Number, and are they applicable on an empty set.
/// All, Any, None are Truth aggregations, and are applicable over an empty set.
/// Sum, Product, Count are Number aggregations, and are applicable over an empty set.
/// Minimum, Maximum, Average, Median are Number aggregations and are not applicable over an empty set.
type Aggregation = All | Any | None | Sum | Prod | Count | Min | Max | Avg | Med

module Aggregation =
    /// Get the type of an aggregation
    let getType aggregation =
        match aggregation with
        | All | Any | None -> Type.Truth
        | _ -> Type.Number

    /// Gets the zero value for each aggregation, if it has one.
    let zeroValue aggregation =
        match aggregation with
        | All -> Some <| Truth Yes
        | Any -> Some <| Truth No
        | None -> Some <| Truth Yes
        | Sum -> Some <| Number 0M
        | Prod -> Some <| Number 0M
        | Count -> Some <| Number 0M
        | _ -> Option.None

    /// Apply an aggregation. Assumes that all type checking has already been done.
    let apply aggregation values =
        let median list =
            if (List.length list) % 2 = 0 then
                let mid = List.length list / 2

                (list.[mid - 1] + list.[mid]) / 2M
            else
                list.[(List.length list) / 2]

        match aggregation with
        | All -> values |> List.map (Value.getTruth >> Truth.toBool >> not) |> List.isEmpty |> Truth.fromBool |> Truth
        | Any -> values |> List.map (Value.getTruth >> Truth.toBool) |> List.isEmpty |> not |> Truth.fromBool |> Truth
        | None -> values |> List.map (Value.getTruth >> Truth.toBool) |> List.isEmpty |> Truth.fromBool |> Truth
        | Sum -> values |> List.map Value.getNumber |> List.fold (+) 0M |> Number
        | Prod -> values |> List.map Value.getNumber |> List.fold (*) 0M |> Number
        | Count -> values |> List.length |> decimal |> Number
        | Min -> values |> List.map Value.getNumber |> List.min |> Number
        | Max -> values |> List.map Value.getNumber |> List.max |> Number
        | Avg -> values |> List.map Value.getNumber |> List.average |> Number
        | Med -> values |> List.map Value.getNumber |> median |> Number


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
    /// An aggregation over a group of variables.
    | Aggr of Aggregation * VariableMask
    /// An aggregate with a predicate over a group of variables.
    | PredAggr of Aggregation * VariableMask * Predicate
    /// A grouping of an expression.
    | Grp of Expression
    /// A variable.
    | Var of string
    /// A number.
    | Num of decimal
    /// A boolean value.
    | Truth of Truth

/// A predicate compares something out against an expression. For numbers, a comparison is provided, for Truths, they
// are compared to Yes.
and Predicate =
    | NumberPredicate of Comparison * Expression
    | TruthPredicate of Expression


/// The state allows expressions to be bound to variables.
type State = Map<string, Expression>

module State =
    /// Lookup a variable in the state.
    let lookup (state:State) var =
        state |> Map.tryFind var

    let lookupMask (state: State) mask =
        state |> Map.filter (fun k _ -> VariableMask.matches mask k) |> Map.toList

module Expression =
    /// Indicates whether an expression is binary or not.
    let isBinary expression =
        match expression with
        | Grp _ | Var _ | Num _ -> false
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
        | Aggr (a, m) -> sprintf "[ %A | %s ]" a (VariableMask.display m)
        | PredAggr (a, m, p) -> sprintf "[ %A | %s | %s ]" a (VariableMask.display m) (displayPredicate p)
        | Grp s -> sprintf "( %s )" (display  s)
        | Var v -> v
        | Num n -> sprintf "%M" n
        | Truth t -> sprintf "%A" t

    /// Display a predicate.
    and displayPredicate predicate =
        match predicate with
        | NumberPredicate (c, e) -> sprintf "%s %s" (Comparison.display c) (display e)
        | TruthPredicate e -> display e

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
            | Aggr (a, m) -> Aggr (a, m)
            | PredAggr(a, m, p) ->
                match p with
                | TruthPredicate e -> PredAggr (a, m, TruthPredicate (expand' state e false))
                | NumberPredicate (c, e) -> PredAggr(a, m, NumberPredicate (c, (expand' state e false)))
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
        let determineMaskType aType m a mt =
            if Option.map (fun t -> aType = t) mt |> Option.defaultValue true then
                Ok aType
            else
                Result.fail (sprintf "Conflicting types of aggregation %A and variable mask %s" a (VariableMask.display m))

        match expression with
        | Div _ | Mul _ | Rem _ | Add _ | Sub _ | Num _ -> Ok Type.Number
        | Com _ -> Ok Type.Truth
        | Grp s -> getType state s
        | Var v -> State.lookup state v |> Option.map (getType state) |> Result.fromOption (fun () -> sprintf "Unbound variable %s" v)
        | Truth _ -> Ok Type.Truth
        | Aggr (a, m) ->
            let aType = Aggregation.getType a
            getMaskType state m |> Result.bind (determineMaskType aType m a)
        | PredAggr (a, m, _) ->
            let aType = Aggregation.getType a

            if aType = Type.Number then
                Result.fail "Pedicate aggregations must have a truth aggregation"
            else
                getMaskType state m |> Result.bind (determineMaskType aType m a)


    // Get the type of a variable mask.
    and getMaskType state mask : Result<Type option, Set<string>> =
        let vars = VariableMask.getVariables state mask |> Map.toList |> List.map snd
        let types = List.map (getType state) vars |> Result.aggregate |> Result.map List.distinct

        types |> Result.bind
            (fun ts ->
               if List.length ts = 0 then
                   Ok Option.None
               elif List.length ts > 1 then
                   Result.fail <| sprintf "Variable mask %s does not identify a single type" (VariableMask.display mask)
               else
                Ok <| Some ts.[0]
            )

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

        /// Handle an aggregation without predicate.
        let handleSimpleAggregation aggregation mask : Result<Value, Set<string>> =
            match getMaskType state mask with
            | Error e -> Error e
            | Ok maskType ->
                let aggrType = Aggregation.getType aggregation
                if maskType |> Option.map (fun t -> t <> aggrType) |> Option.defaultValue false then
                    Result.fail (sprintf "Type %A of aggregation %A is not compatible with type %A of mask %s" (aggrType) (aggregation) (maskType) (VariableMask.display mask))
                else
                    match maskType with
                    | Option.None ->
                        // Empty list of variables.
                        Aggregation.zeroValue aggregation
                            |> Option.map Ok
                            |> Result.fromOption (fun () -> sprintf "Aggregation %A over an empty set of variables is not possible." aggrType)
                    | _ ->
                        // Non-empty list, and all variables are of the same type.
                        VariableMask.getVariables state mask |> Map.toList
                            |> List.map snd |> List.map (evaluate state) |> Result.aggregate
                            |> Result.map (Aggregation.apply aggregation)

        /// Handle an aggregation with a predicate.
        let handlePredicateAggregation aggregation mask pred : Result<Value, Set<string>> =
            let applyPredAggregation maskType predicate aggregation values =
                let evaluatePredicateExpression p =
                    match p with
                    | NumberPredicate (_, e)
                    | TruthPredicate e -> evaluate state e

                let evaluateAggregation f =
                    match aggregation with
                    | All ->
                        List.map (f >> (Result.map (Truth.toBool >> not))) values
                            |> Result.aggregate |> Result.map (List.filter id >> List.isEmpty >> Truth.fromBool >> Value.Truth)
                    | Any ->
                        List.map (f >> (Result.map Truth.toBool)) values |> Result.aggregate
                            |> Result.map (List.filter id >> List.isEmpty >> not >> Truth.fromBool >> Value.Truth)
                    | None ->
                        List.map (f >> (Result.map Truth.toBool)) values |> Result.aggregate
                            |> Result.map (List.filter id >> List.isEmpty >> Truth.fromBool >> Value.Truth)
                    | _ -> Result.fail (sprintf "Invalid predicate aggregation type: %A" aggregation)

                Result.bind
                    (fun value ->
                        match predicate with
                        | NumberPredicate (comp, _) ->
                            match maskType with
                            | Type.Number -> evaluateAggregation (fun varVal -> Comparison.compare comp varVal value |> Result.map Value.getTruth)
                            | _ -> Result.fail "A number predicate cannot work with truth masks."
                        | TruthPredicate _ ->
                            match maskType with
                            | Type.Truth -> evaluateAggregation (fun varVal -> Comparison.compare Comparison.Same varVal value |> Result.map Value.getTruth)
                            | _ -> Result.fail "A truth predicate cannot work with number masks."
                    )
                    (evaluatePredicateExpression predicate)

            Result.bind
                (fun maskType ->
                    let aggrType = Aggregation.getType aggregation
                    if aggrType <> Type.Truth then
                        Result.fail "Predicate aggregation only supports truth."
                    else
                        match maskType with
                        | Option.None ->
                            // Empty list of variables.
                            Aggregation.zeroValue aggregation
                                |> Option.map Ok
                                |> Result.fromOption (fun () -> failwith  "Type check should have been done before")
                        | Option.Some mt ->
                            // Non-empty list, and all variables are of the same type.
                            VariableMask.getVariables state mask |> Map.toList
                                |> List.map snd |> List.map (evaluate state) |> Result.aggregate
                                |> Result.bind (fun values -> applyPredAggregation mt pred aggregation values)

                )
                (getMaskType state mask)


        match expression with
        | Sub (a, b) -> Result.bind2 (ifNum (Result.hoist2 (-))) (evaluate state a) (evaluate state b)
        | Add (a, b) -> Result.bind2 (ifNum (Result.hoist2 (+))) (evaluate state a) (evaluate state b)
        | Rem (a, b) -> Result.bind2 (ifNum (Result.hoist2 (%))) (evaluate state a) (evaluate state b)
        | Mul (a, b) -> Result.bind2 (ifNum (Result.hoist2 (*))) (evaluate state a) (evaluate state b)
        | Div (a, b) -> Result.bind2 (ifNum safeDiv) (evaluate state a) (evaluate state b)
        | Com (c, a, b) -> matchTypes state (Comparison.compare c) a b
        | Aggr (a, m) -> handleSimpleAggregation a m
        | PredAggr (a, m, p) -> handlePredicateAggregation a m p
        | Grp (g) -> evaluate state g
        | Num n -> Ok (Value.Number n)
        | Truth t -> Ok (Value.Truth t)
        | Var v -> State.lookup state v |> Option.map (evaluate state) |> Result.fromOption (fun () -> sprintf "Unbound variable: %s." v)

    /// Return all variables used in an expression.
    let rec usedVars state expression : Set<string> =
        match expression with
        | Div (a, b)
        | Mul (a, b)
        | Rem (a, b)
        | Add (a, b)
        | Sub (a, b)
        | Com (_,a , b) -> Set.union (usedVars state a) (usedVars state b)
        | Aggr (_, m) -> VariableMask.getVariables state m |> Map.toList |> List.map fst |> Set.ofList
        | PredAggr (_, m, p) ->
            let maskVars = VariableMask.getVariables state m |> Map.toList |> List.map fst |> Set.ofList
            match p with
            | TruthPredicate e
            | NumberPredicate (_, e) -> Set.union maskVars (usedVars state e)
        | Grp s -> usedVars state s
        | Var v -> Set.add v ((State.lookup state v) |> Option.map (usedVars state) |> Option.defaultValue Set.empty)
        | Num _ -> Set.empty
        | Truth _ -> Set.empty

/// A statement represents an action that can be performed.
type Statement =
    /// Binding of an expression to a variable.
    | Binding of string * Expression
    /// Clearing one, or all variable bindings.
    | Clear of VariableMask option
    /// Calculation of an expression.
    | Calculation of Expression
    /// Display all variables covered by a mask.
    | MaskDisplay of VariableMask
    /// Exit the program.
    | Exit
    /// Save the current state to a file.
    | Save of string
    /// Load new state from a file.
    | Load of string