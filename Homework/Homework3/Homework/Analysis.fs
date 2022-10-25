module Analysis

open PetriNet

// ----- Utility functions ------------------------------------------------------------------------------------------ //

/// Returns a new marking containing 'ω' where necessary if the set of predecessors received as argument contains
/// another marking that is smaller than or equal to the one received as input.
let setOmegas (predecessors: Set<Marking<'Place>>) (marking: Marking<'Place>) : Marking<'Place> =
    // Try to find a smaller marking in the set of predecessors.
    let smallerPredecessor =
        predecessors
        |> Set.filter (fun predecessor -> predecessor <= marking)
        |> Set.toList
        |> List.tryHead

    match smallerPredecessor with
    // If one is found, set the token counts to ω in places where the predecessor is smaller than the input marking.
    | Some marking' ->
        marking
        |> Marking.map (fun place count ->
            if marking'[place] < count then
                Omega
            else
                count)
    | None -> marking

// ----- CoverabilityGraph ------------------------------------------------------------------------------------------ //

/// A coverability graph is represented by its root (an inital marking) and a set of edges implemented as a mapping from
/// markings to mappings from transitions to markings.
type CoverabilityGraph<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    { Root: Marking<'Place>
      Edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>> }

[<RequireQualifiedAccess>]
module CoverabilityGraph =

    /// Builds the coverability graph for a model, given some initial marking.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : CoverabilityGraph<'Place, 'Transition> =

        // We define a mutable map that records the set of predecessors in the coverability graph for each marking.
        // At the start of the construction of the graph, the map is initialised with an empty set of predecessors for
        // the root marking.
        let mutable predecessorMap =
            Map.add marking Set.empty Map.empty

        /// This function returns a mapping from transitions to markings corresponding to the successors for the marking
        /// it receives as input.
        /// The function should use the predecessors map defined above to set the omegas in the successors where necessary.
        /// It should also update the predecessors map with each new marking it encounters in the graph.
        let successors marking =
            Model.getFireable model marking
            |> Set.fold
                (fun successors transition ->
                    let predecessors =
                        Set.add marking (Map.find marking predecessorMap)

                    let successor =
                        setOmegas predecessors (Model.fire model marking transition).Value

                    predecessorMap <- Map.add successor predecessors predecessorMap

                    Map.add transition successor successors)
                Map.empty

        /// This function builds the coverability graph recursively, until it reaches a fixpoint.
        let rec fixpoint markings edges =
            let edges' =
                markings
                |> Set.fold (fun edges' marking -> Map.add marking (successors marking) edges') edges

            let visitedMarkings, allMarkings =
                edges'
                |> Map.fold
                    (fun (visitedMarkings, allMarkings) marking successors ->
                        (Set.add marking visitedMarkings, Set.union (Set.ofSeq (Map.values successors)) allMarkings))
                    (Set.empty, Set.empty)

            let markings' =
                Set.difference allMarkings visitedMarkings

            if Set.isEmpty markings' then
                edges'
            else
                fixpoint markings' edges'

        { Root = marking
          Edges = fixpoint (Set.singleton marking) Map.empty }

    /// Returns the set of all the markings in some coverability graph.
    let markings (graph: CoverabilityGraph<'Place, 'Transition>) : Set<Marking<'Place>> =
        graph.Edges
        |> Map.fold
            (fun markings _ successors -> Set.union (Set.ofSeq (Map.values successors)) markings)
            (Set.ofSeq (Map.keys graph.Edges))

    /// Returns the total number of markings in a coverability graph.
    let count (graph: CoverabilityGraph<'Place, 'Transition>) : int = markings graph |> Set.count

    /// Checks if there exists a marking in a coverability graph that satisfies some predicate.
    let exists (predicate: Marking<'Place> -> bool) (graph: CoverabilityGraph<'Place, 'Transition>) : bool =
        Set.exists predicate (markings graph)

    /// Returns the set of markings in a coverability graph that satisfy some predicate.
    let filter
        (predicate: Marking<'Place> -> bool)
        (graph: CoverabilityGraph<'Place, 'Transition>)
        : Set<Marking<'Place>> =
        Set.filter predicate (markings graph)
