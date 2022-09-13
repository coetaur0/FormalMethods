open PetriNet
open Analysis

type Place =
    | P1
    | P2
    | P3
    | P4

type Transition =
    | T1
    | T2
    | T3

let pre =
    Arcs.make [ ((P1, T1), 2)
                ((P2, T2), 1)
                ((P3, T3), 3)
                ((P4, T3), 1) ]

let post =
    Arcs.make [ ((P2, T1), 1)
                ((P3, T2), 3)
                ((P4, T2), 1)
                ((P1, T3), 2) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P1, 2) ]

let markingGraph =
    MarkingGraph.make model initialMarking

printfn $"{markingGraph}"
