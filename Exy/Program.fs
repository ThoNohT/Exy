﻿open System
open System.IO
module P = Parsing
open Model

let print txt = printf "%s" txt
let printn txt = printfn "%s" txt

/// Asks for confirmation, and keeps asking until a proper answer is provided.
let rec askConfirmation () =
    let input = Console.ReadLine ()
    let result = P.parseLine P.confirmation input
    match result with
    | Ok r -> r
    | Error _ ->
        print "Unrecognized answer "
        askConfirmation ()

/// Saves the state to the provided filename, asks for confirmation if the file would be overwritten.
let saveState (state: State) fileName =
    let save () =
        let fileContents =
            state |> Map.toList
            |> List.map (fun (k, e) -> sprintf "%s = %s" k (displayExpr e))
        File.WriteAllLines (fileName, fileContents)
        printfn "Saved %s." fileName

    if File.Exists fileName then
        print "File already exists: overwrite? (y/n) "
        if askConfirmation () = Yes then save ()
    else
        save ()

/// Handles a binding, returning an error if a binding is not possible.
let handleBinding state statement =
    match statement with
    | Binding (var, expression) ->
        let used = usedVars state expression
        if Set.contains var used then
            Error <| sprintf "Binding %s to variable %s would create a circular reference." var (displayExpr expression)
        else
            state |> Map.remove var |> Map.add var expression |> Ok
    | _ ->
        Error "Statement is not a binding."

/// Loads new state from a filename. Returning the new state if succesful, or an error with an error message if not.
let loadState fileName : Result<State,string> =
    if not <| File.Exists fileName then
        Error <| sprintf "File %s does not exist." fileName
    else
        let lines = File.ReadAllLines fileName |> List.ofArray

        let folder acc curr =
            Result.bind
                (fun st -> P.parseLine P.statement curr |> Result.bind (handleBinding st))
                acc

        List.fold folder (Result.Ok Map.empty) lines

/// Handle a statement.
let rec handleStatement (state: State) statement =
    match statement with
    | Clear n ->
        match n with
        | Some v -> state |> Map.remove v
        | None -> Map.empty

    | Exit ->
        Environment.Exit 0
        state

    | Load fileName ->
        loadState fileName
        |> Result.showOnOk (sprintf "%s succesfully loaded." fileName)
        |> Result.valueOrShowWithDefault state (sprintf "Error loading %s: " fileName)

    | Save fileName ->
        saveState state fileName
        state

    | Binding _ ->
        handleBinding state statement
        |> Result.valueOrShowWithDefault state "Binding error: "

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
    P.parseLine P.statement input
        |> Result.map (handleStatement state)
        |> Result.valueOrShowWithDefault state ""

/// The main program entry point.
[<EntryPoint>]
let main _ =
    let mutable state = Map.empty
    while true do
        state <- iteration state

    0
