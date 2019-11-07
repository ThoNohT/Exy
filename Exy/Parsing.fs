module Parsing

open Model
open FParsec

module P = FParsec.CharParsers
module P = FParsec.Primitives
module P = FParsec.Internals

/// Parse and ignore whitespace characters.
let private ws = P.unicodeSpaces

/// Parse a numeric character.
let private numeric =
    parse {
        let! r = P.regex "[0-9]"
        return r.Chars 0
    }

/// Parse an alphanumeric character.
let private alphaNumeric = P.asciiLetter <|> numeric

/// parse a file name, formatted as filename.exy
let private fileName : Parser<string, unit> =
    P.many1Chars alphaNumeric .>>. P.pstring ".exy" |>> (fun (a,b) -> sprintf "%s%s" a b)

/// Parse a string representing a variable name. Starts with a letter and is followed by letters or numbers.
let private varStr =
    P.many1Chars2 P.asciiLetter alphaNumeric

/// Parse a variable name. The same as varStr, but wrapped in a Var.
let private var : Parser<Expression, unit> =
    varStr |>> Var

/// Parse a number, wrapped in a Num. Can be a positive or negative number, with or without decimal separator '.'.
let private number : Parser<Expression, unit> =
    P.numberLiteral (NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction) "number"
    |>> (fun l -> Num <| decimal l.String)

/// Wrap another parser between '(' and ')'.
let private group subParser : Parser<Expression, unit> =
    P.between (P.pchar '(' .>> ws) (P.pchar ')' .>> ws) (subParser .>> ws)
    |>> Grp

/// Parse an expression. Can either be a variable, a number, an operator, or a grouped expression.
let private expression : Parser<Expression, unit> =
    let opp = new OperatorPrecedenceParser<Expression, string, unit>()
    let allowSpacesThenEmpty = ws >>. P.stringReturn "" ""

    let newOp op prio mapping = InfixOperator (op, allowSpacesThenEmpty, prio, Associativity.Left, mapping)

    opp.TermParser <- (var <|> number <|> (group <| opp.ExpressionParser)) .>> ws

    opp.AddOperator <| newOp "-" 1 (fun left right -> Sub (left,right))
    opp.AddOperator <| newOp "+" 1 (fun left right -> Add (left,right))
    opp.AddOperator <| newOp "%" 2 (fun left right -> Rem (left,right))
    opp.AddOperator <| newOp "*" 2 (fun left right -> Mul (left,right))
    opp.AddOperator <| newOp "/" 2 (fun left right -> Div (left,right))

    opp.ExpressionParser

/// Parse a binding, format: varStr = expression.
let private binding : Parser<Statement, Unit> =
    parse {
        let! var = varStr
        do! ws .>> P.pchar '=' .>> ws
        let! expr = expression

        return Binding (var, expr)
    }

/// Parse a clear satement, format: clear varStr.
let private clear : Parser<Statement, Unit> =
    P.skipString "clear" .>>. ws >>. varStr
    |>> Clear

/// Parse an exit statement, format: exit.
let private exit : Parser<Statement, Unit> =
    P.skipString "exit" .>>. ws
    |>> (fun _ -> Exit)

// Parse a save statement, format: save filename.
let private save : Parser<Statement, Unit> =
    P.skipString "save" .>>. ws >>. fileName
    |>> Save

// Parse a load statement, format: load filename.
let private load : Parser<Statement, Unit> =
    P.skipString "load" .>>. ws >>. fileName
    |>> Load

/// Parse a statement. Can either be an binding, or an expression.
let statement : Parser<Statement, unit> =
    let calculation = expression |>> Calculation

    (P.attempt exit)
    <|> (P.attempt save)
    <|> (P.attempt load)
    <|> (P.attempt clear)
    <|> (P.attempt binding)
    <|> calculation

/// Parses a confirmation message. Can be either y/n/yes/no, case insensitive.
let confirmation : Parser<Confirmation, unit> =
    (P.regex "[Yy]([Ee][Ss])?" |>> (fun _ -> Yes))
    <|> (P.regex "[Nn][Oo]?" |>> (fun _ -> No))

/// Run a parser, allowing whitespace before and after the parser, requiring the entire string to match.
/// Returns the result of running the provided parser on the provided input.
let parseLine parser input =
    P.run (ws >>. parser .>> ws .>> P.eof) input
    |> function
        | P.Success (r, _, _) -> Result.Ok r
        | P.Failure (e, _, _) -> Result.Error e