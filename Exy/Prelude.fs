[<AutoOpen>]
module Prelude

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