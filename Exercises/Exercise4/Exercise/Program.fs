open PetriNet

// Below is an implementation of the model presented on page 21 of the 'Présentation des propriétés' slides you saw
// during the lessons.

type Place =
    | P0
    | P1
    | P2

type Transition =
    | T0
    | T1
    | T2
    | T3

let pre =
    Arcs.make [ ((P0, T0), 1)
                ((P0, T1), 2)
                ((P1, T3), 1)
                ((P2, T0), 1)
                ((P2, T2), 1) ]

let post =
    Arcs.make [ ((P0, T0), 3)
                ((P1, T1), 1)
                ((P1, T2), 1)
                ((P2, T3), 1) ]

let model = Model.make pre post

// We can play with the model's initial marking to see when it's alive and when it isn't.
let initialMarking =
    Marking.make [ (P0, 5); (P2, 1) ]

// We check if the model is alive by checking the liveness of all of its transitions.
printfn $"Transition T0 is alive: {Analysis.isAlive model initialMarking T0}"
printfn $"Transition T1 is alive: {Analysis.isAlive model initialMarking T1}"
printfn $"Transition T2 is alive: {Analysis.isAlive model initialMarking T2}"
printfn $"Transition T3 is alive: {Analysis.isAlive model initialMarking T3}"
printfn $"The model is alive: {Set.forall (Analysis.isAlive model initialMarking) (Model.transitions model)}"
