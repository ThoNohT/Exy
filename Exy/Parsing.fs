module Parsing

open Model
open FParsec

module P = FParsec.CharParsers
module P = FParsec.Primitives
module P = FParsec.Internals

/// Parse and ignore whitespace characters.
let private ws = P.unicodeSpaces
/// Parse and ignore whitespace characters (at least 1).
let private ws1 = P.unicodeSpaces1

/// Parse a numeric character.
let private numeric =
    parse {
        let! r = P.regex "[0-9]"
        return r.Chars 0
    }

/// Parse an alphanumeric character.
let private alphaNumeric = P.asciiLetter <|> numeric

/// Parse a variable character. Can be alphanumeric, or an underscore.
let private varChar = alphaNumeric <|> P.pchar '_'

/// Parse a file name, formatted as filename.exy
let private fileName : Parser<string, unit> =
    P.many1Chars alphaNumeric .>>. P.pstring ".exy" |>> (fun (a,b) -> sprintf "%s%s" a b)

/// Parse a truthness value.
let private truthness : Parser<Expression, unit> =
    P.attempt <|
        (P.pstring "Yes" |>> (fun _ -> Truth Yes)) <|> (P.pstring "No" |>> (fun _ -> Truth No))

/// Parse a string representing a variable name. Starts with a letter and is followed by letters or numbers.
let private varStr =
    let restrictedNames = [ "Yes"; "No" ]

    parse {
        let! name = P.many1Chars2 P.asciiLetter varChar

        if List.contains name restrictedNames then
            return! P.fail "Restricted variable name"
        else
            return name
    }

/// Parse a variable name. The same as varStr, but wrapped in a Var.
let private var : Parser<Expression, unit> =
    varStr |>> Var

/// Parse a variable mask. A varStr, but with the '*' character included.
let private varMask : Parser<VariableMask, unit> =
    let part : Parser<MaskPart, unit> =
        (P.pchar '*' |>> (fun _ -> Wildcard))
        <|> P.attempt (P.many1 varChar |>> (string >> Literal))

    P.many1 part |>> VariableMask


/// Parse a number, wrapped in a Num. Can be a positive or negative number, with or without decimal separator '.'.
let private number : Parser<Expression, unit> =
    P.numberLiteral (NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction) "number"
    |>> (fun l -> Num <| decimal l.String)

/// Wrap another parser between '(' and ')'.
let private group subParser : Parser<Expression, unit> =
    P.between (P.pchar '(' .>> ws) (P.pchar ')' .>> ws) (subParser .>> ws)
    |>> Grp

/// An aggregation type for truths.
let truthAggregationType =
    P.attempt <|
        (P.attempt <| P.pstring "All" |>> (fun _ -> All))
        <|> (P.attempt <| P.pstring "Any" |>> (fun _ -> Any))
        <|> (P.attempt <| P.pstring "None" |>> (fun _ -> None))

/// An aggregation type for numbers.
let numAggregationType =
        (P.attempt <| P.pstring "Sum" |>> (fun _ -> Sum))
        <|> (P.attempt <| P.pstring "Prod" |>> (fun _ -> Prod))
        <|> (P.attempt <| P.pstring "Count" |>> (fun _ -> Count))
        <|> (P.attempt <| P.pstring "Min" |>> (fun _ -> Min))
        <|> (P.attempt <| P.pstring "Max" |>> (fun _ -> Max))
        <|> (P.attempt <| P.pstring "Avg" |>> (fun _ -> Avg))
        <|> (P.attempt <| P.pstring "Med" |>> (fun _ -> Med))

/// Any type of aggregration type.
let aggregationType =
    (P.attempt truthAggregationType) <|> (P.attempt numAggregationType)

/// A comparison operator
let comparisonOperator : Parser<Comparison, unit> =
        (P.pstring "=" |>> (fun _ -> Same))
        <|> (P.pstring "<" |>> (fun _ -> Lt))
        <|> (P.pstring ">" |>> (fun _ -> Gt))
        <|> (P.attempt <| P.pstring "<=" |>> (fun _ -> Lts))
        <|> (P.attempt <| P.pstring ">=" |>> (fun _ -> Gts))

/// Parse an expression.
let rec private expression : Parser<Expression, unit> =
    let opp = new OperatorPrecedenceParser<Expression, string, unit>()
    let allowSpacesThenEmpty = ws >>. P.stringReturn "" ""

    let newlOp op prio mapping = InfixOperator (op, allowSpacesThenEmpty, prio, Associativity.Left, mapping)
    let newnOp op prio mapping = InfixOperator (op, allowSpacesThenEmpty, prio, Associativity.None, mapping)

    opp.TermParser <- (truthness <|> var <|> number <|> aggregation <|> (group <| opp.ExpressionParser)) .>> ws

    opp.AddOperator <| newnOp "=" 1 (fun left right -> Com (Same, left,right))
    opp.AddOperator <| newnOp ">" 1 (fun left right -> Com (Gt, left,right))
    opp.AddOperator <| newnOp "<" 1 (fun left right -> Com (Lt, left,right))
    opp.AddOperator <| newnOp "<=" 1 (fun left right -> Com (Lts, left,right))
    opp.AddOperator <| newnOp ">=" 1 (fun left right -> Com (Gts, left,right))
    opp.AddOperator <| newlOp "-" 2 (fun left right -> Sub (left,right))
    opp.AddOperator <| newlOp "+" 2 (fun left right -> Add (left,right))
    opp.AddOperator <| newlOp "%" 3 (fun left right -> Rem (left,right))
    opp.AddOperator <| newlOp "*" 3 (fun left right -> Mul (left,right))
    opp.AddOperator <| newlOp "/" 3 (fun left right -> Div (left,right))

    opp.ExpressionParser

/// Parse an aggregation, can be either simple or with a predicate.
and aggregation : Parser<Expression, unit> =
    let simpleAggr =
        parse {
            do! P.pchar '[' >>. ws
            let! typ = aggregationType
            do! ws >>. P.pchar '|' >>. ws
            let! msk = varMask
            do! ws .>> P.pchar ']'

            return Aggr (typ, msk)
        }

    let predAggr =
        parse {
            do! P.pchar '[' >>. ws
            let! typ = truthAggregationType
            do! ws >>. P.pchar '|' >>. ws
            let! msk = varMask
            do! ws >>. P.pchar '|' >>. ws
            let! pred = predicate
            do! ws .>> P.pchar ']'

            return PredAggr (typ, msk, pred)
        }

    (P.attempt simpleAggr) <|> (P.attempt predAggr)

/// Parse a predicate, can be either a numeric predicate, with a comparison operator, or a simple truth predicate.
and predicate : Parser<Predicate, Unit> =
    let numPred =
        parse {
            let! comp = comparisonOperator
            do! ws
            let! expr = expression

            return NumberPredicate (comp, expr)
        }

    let truthPred =
        parse {
            let! expr = expression

            return TruthPredicate expr
        }

    (P.attempt numPred) <|> (P.attempt truthPred)

/// Parse a binding, format: varStr = expression.
let private binding : Parser<Statement, Unit> =
    parse {
        do! P.pstring "let" >>. ws1
        let! var = varStr
        do! ws .>> P.pchar '=' >>. ws
        let! expr = expression

        return Binding (var, expr)
    }

/// Parse a clear satement, format: clear varStr. Or just clear.
let private clear : Parser<Statement, Unit> =
    P.skipString "clear" .>>. ws >>. P.opt (varMask)
    |>> Clear

/// Parse an exit statement, format: exit.
let private exit : Parser<Statement, Unit> =
    P.skipString "exit" .>>. ws
    |>> (fun _ -> Exit)

/// Parse a save statement, format: save filename.
let private save : Parser<Statement, Unit> =
    P.skipString "save" .>>. ws >>. fileName
    |>> Save

/// Parse a load statement, format: load filename.
let private load : Parser<Statement, Unit> =
    P.skipString "load" .>>. ws >>. fileName
    |>> Load

/// Parse a variable mask display statement.
let private maskDisplay : Parser<Statement, Unit> =
    P.skipString "mask" .>>. ws >>. varMask |>> MaskDisplay

/// Parse a statement. Can either be an binding, or an expression.
let statement : Parser<Statement, unit> =
    let calculation = expression |>> Calculation

    (P.attempt exit)
    <|> (P.attempt save)
    <|> (P.attempt load)
    <|> (P.attempt clear)
    <|> (P.attempt binding)
    <|> (P.attempt maskDisplay)
    <|> calculation

/// Parses a confirmation message. Can be either y/n/yes/no, case insensitive.
let confirmation : Parser<Truth, unit> =
    (P.regex "[Yy]([Ee][Ss])?" |>> (fun _ -> Yes))
    <|> (P.regex "[Nn][Oo]?" |>> (fun _ -> No))

/// Run a parser, allowing whitespace before and after the parser, requiring the entire string to match.
/// Returns the result of running the provided parser on the provided input.
let parseLine parser input =
    P.run (ws >>. parser .>> ws .>> P.eof) input
    |> function
        | P.Success (r, _, _) -> Result.Ok r
        | P.Failure (e, _, _) -> Result.Error e