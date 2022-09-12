open PetriNet

// ----- Model definition ------------------------------------------------------------------------------------------- //

type Place =
    | P1
    | P2
    | P3

type Transition =
    | T1
    | T2

let pre =
    Arcs.make [ ((P1, T1), 2)
                ((P2, T1), 1)
                ((P3, T2), 2) ]

let post =
    Arcs.make [ ((P1, T1), 1)
                ((P3, T1), 2) ]

let model = Model.make pre post

printfn "# ----- Model ---------- #"

printfn "Places:"

for place in Model.places model do
    printfn $"{place}"

printfn "Transitions:"

for transition in Model.transitions model do
    printfn $"{transition}"

printfn "# ---------------------- #"

// ----- Initial marking -------------------------------------------------------------------------------------------- //

let initialMarking = Marking.make [ (P1, 2); (P2, 1) ]

printfn $"\nInitial marking: {Marking.toString initialMarking}"

printfn "\nTransitions fireable from the initial marking:"

for transition in Model.getFireable model initialMarking do
    printfn $"{transition}"

// ----- Marking after firing T1 ------------------------------------------------------------------------------------ //

let newMarking = Model.fire model initialMarking T1

printfn $"\nMarking after firing T1: {Marking.toString newMarking.Value}"

printfn "\nTransitions fireable from the new marking:"

for transition in Model.getFireable model newMarking.Value do
    printfn $"{transition}"
