open PetriNet

type Place =
    | P0
    | P1
    | P2
    | P3
    | P4

type Transition =
    | T0
    | T1
    | T2
    | T3
    | T4

let pre =
    Arcs.make [ ((P0, T2), 1)
                ((P1, T3), 1)
                ((P2, T2), 1)
                ((P3, T3), 1)
                ((P4, T4), 18) ]

let post =
    Arcs.make [ ((P0, T3), 1)
                ((P1, T2), 1)
                ((P2, T0), 1)
                ((P3, T1), 1)
                ((P4, T2), 6)
                ((P4, T3), 9) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P0, 1) ]

let markingAfter100 =
    Model.simulate model initialMarking 100

printfn $"Marking after firing a 100 transitions: {markingAfter100}"
