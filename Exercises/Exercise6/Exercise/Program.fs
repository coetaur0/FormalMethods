open PetriNet

type Place =
    | B
    | C
    | S0
    | S1
    | S2
    | S3
    | S4

type Transition =
    | T0
    | T1
    | T2
    | T3

let pre =
    Arcs.make [ ((C, T0), 1)
                ((S0, T0), 1)
                ((S4, T0), 1)
                ((S1, T1), 1)
                ((B, T2), 1)
                ((S2, T2), 1)
                ((S4, T2), 1)
                ((S3, T3), 1) ]

let post =
    Arcs.make [ ((B, T0), 1)
                ((S1, T0), 1)
                ((S0, T1), 1)
                ((S4, T1), 1)
                ((C, T2), 1)
                ((S3, T2), 1)
                ((S2, T3), 1)
                ((S4, T3), 1) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (C, 3)
                   (S0, 1)
                   (S2, 1)
                   (S4, 1) ]

printfn $"The model is alive: {Analysis.isAlive model initialMarking}"
printfn $"The model is blockable: {Analysis.isBlockable model initialMarking}"
printfn $"The model is reversible: {Analysis.isReversible model initialMarking}"
