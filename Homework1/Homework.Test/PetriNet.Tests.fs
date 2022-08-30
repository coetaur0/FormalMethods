module PetriNet.Tests

open Xunit
open FsUnit.Xunit

open PetriNet

// ----- Test model ------------------------------------------------------------------------------------------------- //

type Place =
    | P1
    | P2
    | P3

type Transition =
    | T1
    | T2
    | T3

let pre =
    ArcSet.make [ ((P1, T3), 1)
                  ((P2, T3), 1)
                  ((P3, T1), 1) ]

let post =
    ArcSet.make [ ((T1, P1), 1)
                  ((T2, P1), 1)
                  ((T2, P3), 1)
                  ((T3, P2), 2) ]

let model = Model.make pre post

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The 'Model.fireable' function should return true for transitions that are fireable`` () =
    Model.fireable model (Marking.make [ (P1, 1); (P3, 1) ]) T1
    |> should be True

    Model.fireable
        model
        (Marking.make [ (P1, 5)
                        (P2, 10)
                        (P3, 10) ])
        T1
    |> should be True

    Model.fireable model (Marking.make []) T2
    |> should be True

    Model.fireable model (Marking.make [ (P1, 1); (P2, 1) ]) T3
    |> should be True

[<Fact>]
let ``The 'Model.fireable' function should return false for transitions that aren't fireable`` () =
    Model.fireable model (Marking.make [ (P1, 1); (P2, 1) ]) T1
    |> should be False

    Model.fireable model (Marking.make [ (P1, 10); (P2, 10) ]) T1
    |> should be False

    Model.fireable model (Marking.make [ (P2, 1) ]) T3
    |> should be False

    Model.fireable model (Marking.make [ (P1, 1) ]) T3
    |> should be False

[<Fact>]
let ``The 'Model.getFireable' function should return the correct set of fireable transitions`` () =
    Model.getFireable model (Marking.make [])
    |> should equal (Set [ T2 ])

    Model.getFireable model (Marking.make [ (P3, 1) ])
    |> should equal (Set [ T1; T2 ])

    Model.getFireable model (Marking.make [ (P1, 1) ])
    |> should equal (Set [ T2 ])

    Model.getFireable model (Marking.make [ (P1, 1); (P2, 1) ])
    |> should equal (Set [ T2; T3 ])
