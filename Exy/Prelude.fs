[<AutoOpen>]
module Prelude

open System.Text.RegularExpressions

/// Matches a string with a regex.
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

/// Flips a function's parameters.
let flip f a b = f b a

/// Helpers for the Result<'T,'TError> type.
module Result =
    // Note that due to the error collecting behavior implemented in apply, this functionality is no longer consistent
    // with monad. Implementing bind from these methods would work incorrectly due to not short-circuiting.
    // Consider not using the built-in Result type but creating a dedicated type that cannot be used with an expression
    // builder as that needs a proper monad implementation.

    /// Returns an error with a single message
    let fail error =
        Error <| Set.singleton error

    // Apply a function in a result to another result.
    let apply a f =
        match f, a with
        | Ok fv, Ok av -> Ok (fv av)
        | Ok _, Error e
        | Error e, Ok _ -> Error e
        | Error e1, Error e2 -> Error <| Set.union e1 e2

    /// Bind a function on two results.
    let bind2 f a b =
        let tuple a b = (a, b)
        let uncurry f (a, b) = f a b

        let tupled = Result.map tuple a |> (apply b)
        Result.bind <| uncurry f <| tupled

    /// Indicates whether a  result is an error.
    let isError r =
        match r with
        | Ok _ -> false
        | Error _ -> true

    /// Get the value of a result.
    let private value r =
        match r with
        | Ok v -> v
        | Error _ -> failwith "Tried to get result of error"

    /// Get the error of a result.
    let private error r =
        match r with
        | Ok _ -> failwith "Tried to get error of ok"
        | Error e -> e

    /// Aggregate a list of results into a single result. Multiple errors are combined, discarding the succesful results.
    /// If all results are succesful, one Ok is returned.
    let aggregate (rs: Result<'a, Set<string>> list) : Result<'a list, Set<string>> =
        let errors = List.filter isError rs

        if List.isEmpty errors then
            Ok <| List.map value rs
        else
            Error <| (Set.unionMany <| List.map error errors)

    /// Map a function on two results.
    let map2 f a b =
        bind2 (fun av bv -> f av bv |> Ok) a b

    /// Converts an option to a result with the specified error message.
    let fromOption error opt =
        match opt with
        | Some v -> v
        | None -> fail error

    /// Returns the result value. If the result is an error, the error is displayed and a default value is returned.
    let valueOrShowWithDefault defaultValue prefix result =
        match result with
        | Ok r -> r
        | Error e ->
            printfn "%s%s" prefix e
            defaultValue

    /// Returns the result, but displays a message if the result is Ok.
    let showOnOk message result =
        match result with
        | Ok r ->
            printfn "%s" message
            Ok r
        | Error e ->
            Error e

    /// Turns a pure function into a function that returns an Ok result.
    let hoist2 (f: 'a -> 'b -> 'c): ('a -> 'b -> Result<'c, Set<string>>) =
        (fun a b -> Ok <| f a b)