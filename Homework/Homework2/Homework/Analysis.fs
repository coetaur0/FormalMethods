module Analysis

open PetriNet

// ----- Node ------------------------------------------------------------------------------------------------------- //

/// A node in the marking graph of a Petri net is labelled with the marking it represents and is connected to other
/// nodes through edges labelled with transitions. The edges are represented by a mapping from transitions to successor
/// markings.
type Node<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    { Marking: Marking<'Place>
      Edges: Map<'Transition, Marking<'Place>> }

[<RequireQualifiedAccess>]
module Node =

    /// Returns the node computed for a given marking in some model.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Node<'Place, 'Transition> =
        // The outgoing edges for a node are labelled with the transitions that are fireable from its marking. Each edge
        // points to the marking that is obtained after firing the transition it's labelled with.
        let edges =
            Model.getFireable model marking
            |> Set.fold
                (fun edges transition -> Map.add transition (Model.fire model marking transition).Value edges)
                Map.empty

        { Marking = marking; Edges = edges }

    /// Returns the set of all the successor markings for a node.
    let successors (node: Node<'Place, 'Transition>) : Set<Marking<'Place>> =
        node.Edges
        |> Map.fold (fun markings _ marking -> Set.add marking markings) Set.empty

// ----- MarkingGraph ----------------------------------------------------------------------------------------------- //

/// The marking graph of a Petri net is represented by a set of nodes and a root. The root of the graph is the initial
/// marking from which it was computed. The nodes are labelled with markings and are connected to other nodes through
/// edges labelled with transitions.
type MarkingGraph<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    { Root: Marking<'Place>
      Nodes: Set<Node<'Place, 'Transition>> }

[<RequireQualifiedAccess>]
module MarkingGraph =

    /// Builds the marking graph for a model, given some initial marking as its root.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : MarkingGraph<'Place, 'Transition> =
        let rec fixpoint markings nodes =
            // Compute the new nodes for the markings received as argument and add them to the set of nodes in the
            // marking graph.
            let nodes' =
                markings
                |> Set.fold (fun newNodes marking -> Set.add (Node.make model marking) newNodes) nodes

            // The set of markings that have already been visited by the algorithm is the set of markings that label the
            // nodes of the graph. The set of all the successors in the graph is the union of the successors of every
            // node in it. The two sets are computed in parallel with a fold over the nodes in the graph.
            let visited, successors =
                nodes'
                |> Set.fold
                    (fun (visited, successors) node ->
                        (Set.add node.Marking visited, Set.union (Node.successors node) successors))
                    (Set.empty, Set.empty)

            // The set of markings that still need to be visited by the construction algorithm is the difference between
            // the set of all successors and the set of markings that were already visited.
            let markings' =
                Set.difference successors visited

            if Set.isEmpty markings' then
                nodes'
            else
                fixpoint markings' nodes'

        { Root = marking
          Nodes = fixpoint (Set.singleton marking) Set.empty }

    /// Returns the set of all the markings in some marking graph.
    let markings (graph: MarkingGraph<'Place, 'Transition>) : Set<Marking<'Place>> =
        graph.Nodes
        |> Set.map (fun node -> node.Marking)

    /// Returns the total number of nodes in a marking graph.
    let count (graph: MarkingGraph<'Place, 'Transition>) : int = graph.Nodes |> Set.count

    /// Checks if there exists a node in a marking graph which satisfies the input predicate.
    let exists (predicate: Node<'Place, 'Transition> -> bool) (graph: MarkingGraph<'Place, 'Transition>) : bool =
        Set.exists predicate graph.Nodes

    /// Returns the set of nodes in a marking graph that satisfy some predicate.
    let filter
        (predicate: Node<'Place, 'Transition> -> bool)
        (graph: MarkingGraph<'Place, 'Transition>)
        : Set<Node<'Place, 'Transition>> =
        Set.filter predicate graph.Nodes

// ----- Analysis functions ----------------------------------------------------------------------------------------- //

/// Checks if a transition is quasi-alive in a given model for some initial marking.
let isQuasiAlive (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) (transition: 'Transition) : bool =
    MarkingGraph.make model marking
    |> MarkingGraph.exists (fun node -> Seq.contains transition (Map.keys node.Edges))

/// Returns the set of markings that are deadlocks in a given model when transitions are fired starting from some
/// initial marking.
let deadlocks (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<Marking<'Place>> =
    MarkingGraph.make model marking
    |> MarkingGraph.filter (fun node -> Map.isEmpty node.Edges)
    |> Set.map (fun node -> node.Marking)
