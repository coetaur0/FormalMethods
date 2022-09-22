module Parser

open FParsec
open Core

// ----- Parse result ----------------------------------------------------------------------------------------------- //

/// A result obtained after parsing a string.
type ParseResult =
    | Success of Cmd
    | Failure of string

// ----- Literals  -------------------------------------------------------------------------------------------------- //

/// Parses the 'true' boolean literal.
let pTrue =
    skipString "true" >>% Value(Bool true)

/// Parses the 'false' boolean literal.
let pFalse =
    skipString "false" >>% Value(Bool false)

/// Parses an integer literal.
let pInt = pint32 |>> Int |>> Value

/// Parses an identifier string.
let pIdent =
    let identifierStart c = isLetter c || c = '_'
    let identifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L identifierStart identifierChar "identifier"

/// Parses a variable.
let pVar = pIdent |>> Var

// ----- Primary expressions ---------------------------------------------------------------------------------------- //

/// Parses an expression.
let pExpr, pExprRef =
    createParserForwardedToRef ()

/// Parses a parenthesized expression.
let pParen =
    between (pchar '(' .>> spaces) (pchar ')' .>> spaces) pExpr

/// Parses a primary expression.
let pPrimary =
    choice [ pTrue
             pFalse
             pInt
             pVar
             pParen ]

// ----- Unary expressions ------------------------------------------------------------------------------------------ //

/// Parses the 'not' unary operator.
let pNot = skipString "not" >>% Not

/// Parses the 'negation' unary operator.
let pNeg = skipChar '-' >>% Neg

/// Parses a unary expression.
let pUnary, pUnaryRef =
    createParserForwardedToRef ()

do
    pUnaryRef.Value <-
        ((pNot <|> pNeg) .>> spaces .>>. pUnary
         |>> Unary)
        <|> pPrimary

// ----- Binary expressions ----------------------------------------------------------------------------------------- //

/// Creates a parser for a binary expression from a parser for a binary operator, a parser for the left-hand side of
/// the expression and a parser for the right-hand side of the expression.
let pBinary op lhs rhs =
    lhs .>> spaces
    .>>. opt (op .>> spaces .>>. rhs)
    |>> fun (lhs, tail) ->
            match tail with
            | Some (op, rhs) -> Binary(op, lhs, rhs)
            | None -> lhs

/// Parses the binary operator for addition.
let pAdd = pchar '+' >>% Add

/// Parses the binary operator for less than comparison.
let pLess = pchar '<' >>% Less

/// Parses the binary operator for equality tests.
let pEqual = pchar '=' >>% Equal

/// Parses the binary operator for logical and.
let pAnd = skipString "and" >>% And

/// Parses the binary operator for logical or.
let pOr = skipString "or" >>% Or

/// Parses an addition expression.
let pAddExpr, pAddExprRef =
    createParserForwardedToRef ()

/// Parses a less than expression.
let pLessExpr, pLessExprRef =
    createParserForwardedToRef ()

/// Parses an equal expression.
let pEqualExpr, pEqualExprRef =
    createParserForwardedToRef ()

/// Parses a logical and expression.
let pAndExpr, pAndExprRef =
    createParserForwardedToRef ()

/// Parses a logical or expression.
let pOrExpr, pOrExprRef =
    createParserForwardedToRef ()

do
    pAddExprRef.Value <- pBinary pAdd pUnary pAddExpr

    pLessExprRef.Value <- pBinary pLess pAddExpr pLessExpr

    pEqualExprRef.Value <- pBinary pEqual pLessExpr pEqualExpr

    pAndExprRef.Value <- pBinary pAnd pEqualExpr pAndExpr

    pOrExprRef.Value <- pBinary pOr pAndExpr pOrExpr

// ----- If expressions --------------------------------------------------------------------------------------------- //

/// Parses an if expression.
let pIf =
    pipe3
        (skipString "if" >>. spaces >>. pExpr
         .>> spaces)
        (skipString "then" >>. spaces >>. pExpr
         .>> spaces)
        (skipString "else" >>. spaces >>. pExpr)
        (fun cond thn els -> If(cond, thn, els))

do pExprRef.Value <- pIf <|> pOrExpr

// ----- Commands --------------------------------------------------------------------------------------------------- //

/// Parses a variable definition command.
let pLet =
    pipe2
        (skipString "let" >>. spaces >>. pIdent
         .>> spaces
         .>> pchar '='
         .>> spaces)
        pExpr
        (fun ident expr -> Let(ident, expr))

/// Parses an expression evaluation command.
let pExprCmd = pExpr |>> Expr

/// Parses a command.
let pCmd = pLet <|> pExprCmd

// ----- Parser ----------------------------------------------------------------------------------------------------- //

/// Parses a string.
let parse str =
    match run pCmd str with
    | ParserResult.Success (cmd, _, _) -> Success cmd
    | ParserResult.Failure (error, _, _) -> Failure error
