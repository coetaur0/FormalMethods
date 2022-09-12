module PetriNet

open Microsoft.FSharp.Reflection

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the set of all the cases in a discriminated union.
let private getAllCases<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The types for places and transitions must be discriminated unions."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Set.ofSeq

// ----- Token ------------------------------------------------------------------------------------------------------ //

/// Tokens in a Petri net are simply represented by integer counts.
type Token = int

// ----- Marking ---------------------------------------------------------------------------------------------------- //

/// A marking is a mapping from places to token counts.
type Marking<'Place when 'Place: comparison> = private { Counts: Map<'Place, Token> }

[<RequireQualifiedAccess>]
module Marking =

    /// Returns a new marking built from a sequence of mappings from places to token counts.
    let make (mappings: seq<'Place * Token>) : Marking<'Place> =
        let totalMap =
            getAllCases<'Place> ()
            |> Set.fold (fun zeroMap place -> Map.add place 0 zeroMap) Map.empty

        let counts =
            mappings
            |> Seq.fold (fun countMap (place, tokens) -> Map.add place tokens countMap) totalMap

        { Counts = counts }

    /// Returns the token count associated with a given place in a marking.
    let find (place: 'Place) (marking: Marking<'Place>) : Token = Map.find place marking.Counts

    /// Returns a new marking where the token count associated with a place has been updated to a new value.
    let update (place: 'Place) (count: Token) (marking: Marking<'Place>) : Marking<'Place> =
        { Counts = Map.add place count marking.Counts }

    /// Returns a string representation for a marking.
    let toString (marking: Marking<'Place>) : string =
        let repr =
            marking.Counts
            |> Map.fold (fun str place tokens -> str + $"{place}: {tokens}, ") "{"

        repr[0 .. repr.Length - 3] + "}"

// ----- Arcs ------------------------------------------------------------------------------------------------------- //

/// The pre- and post-condition functions of a Petri net are represented by sets of weighted arcs from places to
/// transitions. The pre/post-condition functions map pairs of places and transitions to token counts.
type Arcs<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    private
        { Values: Map<'Place * 'Transition, Token> }

[<RequireQualifiedAccess>]
module Arcs =

    /// Returns a new set of arcs built from a sequence of mappings from pairs of nodes to token counts.
    let make (mappings: seq<('Place * 'Transition) * Token>) : Arcs<'Place, 'Transition> = { Values = Map(mappings) }

    /// Returns the token count associated with a pair of nodes in a set of arcs.
    let find (arc: 'Place * 'Transition) (set: Arcs<'Place, 'Transition>) : Token =
        match Map.tryFind arc set.Values with
        | Some count -> count
        | None -> 0

// ----- Model ------------------------------------------------------------------------------------------------------ //

/// A Petri net is represented by the types of its places and transitions, as well as its pre- and post-condition
/// functions.
type Model<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    private
        { Places: Set<'Place>
          Transitions: Set<'Transition>
          Pre: Arcs<'Place, 'Transition>
          Post: Arcs<'Place, 'Transition> }

[<RequireQualifiedAccess>]
module Model =

    /// Returns a new model built from pre- and post-condition functions expressed as sets of arcs.
    let make (pre: Arcs<'Place, 'Transition>) (post: Arcs<'Place, 'Transition>) : Model<'Place, 'Transition> =
        { Places = getAllCases<'Place> ()
          Transitions = getAllCases<'Transition> ()
          Pre = pre
          Post = post }

    /// Returns the set of places in a model.
    let places (model: Model<'Place, _>) : Set<'Place> = model.Places

    /// Returns the set of transitions in a model.
    let transitions (model: Model<_, 'Transition>) : Set<'Transition> = model.Transitions

    /// Checks if a transition is fireable from a given marking in a model.
    let fireable (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) (transition: 'Transition) : bool =
        model.Places
        |> Set.forall (fun place ->
            (Marking.find place marking)
            - (Arcs.find (place, transition) model.Pre)
            >= 0)

    /// Returns the set of all the transitions that are fireable from a given marking in a model.
    let getFireable (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<'Transition> =
        model.Transitions
        |> Set.filter (fireable model marking)

    /// Fires a transition from a given marking in a model.
    /// Returns some new marking if the transition is fireable, or none otherwise.
    let fire
        (model: Model<'Place, 'Transition>)
        (marking: Marking<'Place>)
        (transition: 'Transition)
        : Option<Marking<'Place>> =
        if not (fireable model marking transition) then
            None
        else
            model.Places
            |> Set.fold
                (fun newMarking place ->
                    Marking.update
                        place
                        ((Marking.find place marking)
                         - (Arcs.find (place, transition) model.Pre)
                         + (Arcs.find (place, transition) model.Post))
                        newMarking)
                marking
            |> Some

    /// Returns the complete state space of a model, starting from some marking.
    let stateSpace (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<Marking<'Place>> =
        let getSuccessors marking =
            getFireable model marking
            |> Set.fold
                (fun successors transition -> Set.add (fire model marking transition).Value successors)
                Set.empty

        let rec fixpoint space =
            let space' =
                space
                |> Set.fold (fun newSpace marking -> Set.union newSpace (getSuccessors marking)) space

            if space = space' then
                space'
            else
                fixpoint space'

        fixpoint (Set [ marking ])
