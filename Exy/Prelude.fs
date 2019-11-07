[<AutoOpen>]
module Prelude

/// Helpers for the Result<'T,'TError> type.
module Result =
    /// Returns an error with a single message
    let fail error =
        Error <| Set.singleton error

    /// Bind a function on two results.
    let bind2 f a b =
        match a, b with
        | Ok av, Ok bv -> f av bv
        | Ok _, Error e -> Error e
        | Error e, Ok _ -> Error e
        | Error e1, Error e2 -> Error <| Set.union e1 e2

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
