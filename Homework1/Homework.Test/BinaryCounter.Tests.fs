module BinaryCounter.Tests

open Xunit
open FsUnit.Xunit

open PetriNet

// ----- State space ------------------------------------------------------------------------------------------------ //

let stateSpace = Model.stateSpace model initialMarking

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The initial marking of the binary counter model should be the state representing number 0`` () =
    Marking.find Bit0 initialMarking |> should equal 0

    Marking.find Bit1 initialMarking |> should equal 0

    Marking.find Bit2 initialMarking |> should equal 0

[<Fact>]
let ``The binary counter model should reach *every* state corresponding to the numbers from 0 to 7`` () =
    for b0 in 0..1 do
        for b1 in 0..1 do
            for b2 in 0..1 do
                Set.exists
                    (fun marking ->
                        (Marking.find Bit0 marking) = b0
                        && (Marking.find Bit1 marking) = b1
                        && (Marking.find Bit2 marking) = b2)
                    stateSpace
                |> should be True

[<Fact>]
let ``The binary counter model should *only* reach the 8 states corresponding to the numbers from 0 to 7`` () =
    stateSpace.Count |> should equal 8
