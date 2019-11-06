open System
module P = Parsing
open Model

// TODO: Load a file, serialize as a list of bindings that are just evaluated in turn and will also fail like manually entered ones.
//       => This means no cycle detection is needed upon evaluation.

let print txt = printf "%s" txt
let printn txt = printfn "%s" txt

/// Handle a statement.
let rec handleStatement (state: State) statement =
    match statement with
    | Clear n ->
        state |> Map.remove n
    | Exit ->
        Environment.Exit 0
        state
    | Load fileName ->
        state // TODO: import state by folding over statements to execute.

    | Save fileName ->
        System.IO.File.WriteAllLines (fileName, state |> Map.toList |> List.map (fun (k, e) -> sprintf "%s = %s" k (displayExpr e)))
        state
    | Binding (n, e) ->
        let used = usedVars state e
        if Set.contains n used then
            printfn "Binding %s to variable %s would create a circular reference." n (displayExpr e)
            state
        else
            state |> Map.remove n |> Map.add n e
    | Calculation e ->
        let varVal = expandVariable state e
        let expanded = expand state e

        // Display the expanded value of a variable.
        printfn "Value: %s" (displayExpr varVal)

        // Display the completely expanded expression, if different than the variable value.
        if expanded <> varVal then printfn "Expands to %s" (displayExpr expanded)

        // Evaluate the expression.
        match evaluate state e with
        | Ok  n -> printfn "Evaluates to %M" n
        | Error e -> printfn "No evaluation possible: %s" (String.Join (", ", e))
        state

/// The main program logic, parses and runs a line, returning the new state.
let iteration state: State =
    print ">"
    let input = Console.ReadLine ()
    let result = P.parseLine P.statement input

    match result with
    | P.Success value ->
        handleStatement state value
    | P.Error e ->
        printfn "Error: %s" e
        state

/// The main program entry point.
[<EntryPoint>]
let main _ =
    let mutable state = Map.empty
    while true do
        state <- iteration state

    0
