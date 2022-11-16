module LinearAlgebra

open Microsoft.FSharp.Reflection

open MathNet.Numerics.LinearAlgebra

open PetriNet

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the values of a discriminated union in a sequence, in the order of their definition.
/// For example, for some 'type Place = P0 | P1 | P2', 'orderedValues<Place> ()' returns the sequence '[P0; P1; P2]'.
let private orderedValues<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The types whose values are being returned must be a discriminated union."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Seq.sort

// ----- Vector ----------------------------------------------------------------------------------------------------- //

/// A vector representation for a marking, a sequence of transition or an invariant.
type Vector = Vector<double>

[<RequireQualifiedAccess>]
module Vector =

    /// Returns the vector representation for a marking.
    let fromMarking (marking: Marking<'Place>) : Vector =
        orderedValues<'Place> ()
        |> Seq.map (fun place -> double marking[place])
        |> vector

    /// Returns the marking corresponding to some vector representation.
    let toMarking (vector: Vector) : Marking<'Place> =
        let places = orderedValues<'Place> ()

        if Vector.length vector
           <> Seq.length places then
            failwith "The input vector does not correspond to a marking!"

        places
        |> Seq.mapi (fun index place -> (place, int vector[index]))
        |> Marking.make

    /// Returns the characteristic vector for a sequence of transitions.
    let fromSequence (sequence: seq<'Transition>) : Vector =
        let transitions =
            orderedValues<'Transition> ()

        let totalMap =
            transitions
            |> Seq.fold (fun map transition -> Map.add transition 0 map) Map.empty

        let counts =
            sequence
            |> Seq.fold (fun counts transition -> Map.add transition ((Map.find transition counts) + 1) counts) totalMap

        transitions
        |> Seq.map (fun transition -> double (Map.find transition counts))
        |> vector

    /// Returns a vector representation for a map from values of some type to integer weights.
    /// This function can be used to build a vector representation for P- and T-invariants.
    let fromMap (map: Map<'T, int>) : Vector =
        orderedValues<'T> ()
        |> Seq.rev
        |> Seq.fold
            (fun vector element ->
                let value =
                    match Map.tryFind element map with
                    | Some value -> value
                    | None -> 0

                double value :: vector)
            []
        |> vector

// ----- Matrix ----------------------------------------------------------------------------------------------------- //

/// A matrix representation for the input/output/incidence matrix of a Petri net.
type Matrix = Matrix<double>

[<RequireQualifiedAccess>]
module Matrix =

    /// Builds a matrix representation for a set arcs between places and transitions.
    let make (arcs: Arcs<'Place, 'Transition>) : Matrix =
        let places =
            orderedValues<'Place> () |> Seq.rev

        let transitions =
            orderedValues<'Transition> () |> Seq.rev

        let buildRow place =
            transitions
            |> Seq.fold (fun row transition -> (double arcs[place, transition]) :: row) []
            |> vector

        places
        |> Seq.fold (fun matrix place -> buildRow place :: matrix) []
        |> matrix

    /// Returns the input matrix for a Petri net model.
    let input (model: Model<'Place, 'Transition>) : Matrix = make (Model.pre model)

    /// Returns the output matrix for a Petri net model.
    let output (model: Model<'Place, 'Transition>) : Matrix = make (Model.post model)

    /// Returns the incidence matrix for a Petri net model.
    let incidence (model: Model<'Place, 'Transition>) : Matrix = (output model) - (input model)

// ----- Fundamental equation --------------------------------------------------------------------------------------- //

/// Returns the marking obtained after firing a sequence of transitions, computed with the fundamental equation.
let markingAfter
    (model: Model<'Place, 'Transition>)
    (marking: Marking<'Place>)
    (sequence: seq<'Transition>)
    : Marking<'Place> =

    (Vector.fromMarking marking)
    + (Matrix.incidence model)
      * (Vector.fromSequence sequence)
    |> Vector.toMarking

// ----- Invariants ------------------------------------------------------------------------------------------------- //

/// Checks if a mapping from places to integer weights is a P-invariant for some model.
let isPInvariant (model: Model<'Place, 'Transition>) (weights: Map<'Place, int>) : bool =
    (Vector.fromMap weights)
    * (Matrix.incidence model)
    |> Vector.forall (fun value -> value = 0)

/// Checks if a mapping from transitions to integer weights is a T-invariant for some model.
let isTInvariant (model: Model<'Place, 'Transition>) (weights: Map<'Transition, int>) : bool =
    (Matrix.incidence model)
    * (Vector.fromMap weights)
    |> Vector.forall (fun value -> value = 0)
