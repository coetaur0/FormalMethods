module Logic

open Microsoft.FSharp.Reflection

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the set of all the cases in a discriminated union.
let private getAllCases<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The type for a set of propositions must be a discriminated union."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Set.ofSeq

// ----- Valuation -------------------------------------------------------------------------------------------------- //

/// A valuation is a mapping from a set of propositions in first-order logic to boolean values.
type Valuation<'Proposition when 'Proposition: comparison> =
    private
        { Values: Map<'Proposition, bool> }

    /// Returns the boolean value associated to some proposition in the valuation.
    member this.Item(proposition: 'Proposition) : bool =
        match Map.tryFind proposition this.Values with
        | Some v -> v
        | None -> false

    /// Returns a string representation for the valuation.
    override this.ToString() : string =
        let repr =
            getAllCases<'Proposition> ()
            |> Set.fold (fun str prop -> str + $"{prop}: {this[prop]}, ") "{"

        repr[0 .. repr.Length - 3] + "}"

module Valuation =

    /// Returns a new valuation built from a mapping from propositions to boolean values. Any proposition omitted from
    /// the mapping is attributed the default value 'false'.
    let make (mapping: seq<'Proposition * bool>) : Valuation<'Proposition> =
        let values =
            mapping
            |> Seq.fold (fun valuation (proposition, truth) -> Map.add proposition truth valuation) Map.empty

        { Values = values }

    /// Returns the list of all possible valuations for a set of propositions.
    let all<'Proposition when 'Proposition: comparison> () : Valuation<'Proposition> list =
        let propositions =
            getAllCases<'Proposition> ()
            |> Set.toList

        let rows =
            [ for i in 1 .. List.length propositions do
                  yield [ true; false ] ]
            |> List.fold
                (fun products list ->
                    [ for boolean in list do
                          for product in products do
                              yield boolean :: product ])
                [ [] ]

        rows
        |> List.map2
            List.zip
            [ for i in 1 .. int (2.0 ** (List.length propositions)) do
                  yield propositions ]
        |> List.map make

// ----- Formula ---------------------------------------------------------------------------------------------------- //

/// A formula in first-order logic.
type Formula<'Proposition when 'Proposition: comparison> =
    | True
    | False
    | Prop of 'Proposition
    | Not of Formula<'Proposition>
    | And of Formula<'Proposition> * Formula<'Proposition>
    | Or of Formula<'Proposition> * Formula<'Proposition>

    /// Returns the negation of a formula.
    static member (~-)(operand: Formula<'Proposition>) : Formula<'Proposition> = Not(operand)

    /// Returns the conjunction of two formulae.
    static member (*)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> = And(lhs, rhs)

    /// Returns the disjunction of two formulae.
    static member (+)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> = Or(lhs, rhs)

    /// Returns an implication between two formulae.
    static member (=>)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> = (-lhs) + rhs

    /// Returns an equivalence between two formulae.
    static member (<=>)(lhs: Formula<'Proposition>, rhs: Formula<'Proposition>) : Formula<'Proposition> =
        (lhs => rhs) * (rhs => lhs)

    /// Returns a string representation for the formula.
    override this.ToString() : string =
        match this with
        | True -> "true"
        | False -> "false"
        | Prop p -> $"{p}"
        | Not f -> $"¬{f}"
        | And (lhs, rhs) -> $"({lhs} /\ {rhs})"
        | Or (lhs, rhs) -> $"({lhs} \/ {rhs})"

module Formula =

    /// Evaluates the truth of a formula, given some valuation for the propositions in it.
    let rec eval (formula: Formula<'Proposition>) (valuation: Valuation<'Proposition>) : bool =
        match formula with
        | True -> true
        | False -> false
        | Prop p -> valuation[p]
        | Not f -> not (eval f valuation)
        | And (lhs, rhs) ->
            (eval lhs valuation)
            && (eval rhs valuation)
        | Or (lhs, rhs) ->
            (eval lhs valuation)
            || (eval rhs valuation)

    /// Checks if a formula is a tautology.
    let isTautology (formula: Formula<'Proposition>) : bool =
        Valuation.all<'Proposition> ()
        |> List.forall (fun valuation -> (eval formula valuation) = true)

    /// Transforms a formula into negative normal form (NNF).
    let rec nnf (formula: Formula<'Proposition>) : Formula<'Proposition> =
        match formula with
        | Not f ->
            match f with
            | Not f' -> nnf f'
            | And (lhs, rhs) -> Or(nnf (Not lhs), nnf (Not rhs))
            | Or (lhs, rhs) -> And(nnf (Not lhs), nnf (Not rhs))
            | _ -> Not f
        | And (lhs, rhs) -> And(nnf lhs, nnf rhs)
        | Or (lhs, rhs) -> Or(nnf lhs, nnf rhs)
        | _ -> formula

    /// Transforms a formula into conjunctive normal form (CNF).
    let cnf (formula: Formula<'Proposition>) : Formula<'Proposition> =
        let rec helper formula =
            match formula with
            | Or (f, And (lhs, rhs))
            | Or (And (lhs, rhs), f) -> And(Or(helper f, helper lhs), Or(helper f, helper rhs))
            | Or (f, Or (lhs, rhs)) -> Or(Or(helper f, helper lhs), helper rhs)
            | And (lhs, rhs) -> And(helper lhs, helper rhs)
            | Or (lhs, rhs) -> Or(helper lhs, helper rhs)
            | _ -> formula

        helper (nnf formula)
