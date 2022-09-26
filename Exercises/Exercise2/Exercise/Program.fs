open PetriNet

/// The places of the Petri net representing a traffic light.
type Place =
    | Red
    | Yellow
    | Green

/// The transitions of the Petri net representing a traffic light.
type Transition =
    | GreenToYellow
    | YellowToRed
    | RedToGreen

/// The Petri net's pre-conditions.
let pre =
    Arcs.make [ ((Green, GreenToYellow), 1)
                ((Yellow, YellowToRed), 1)
                ((Red, RedToGreen), 1) ]

/// The Petri net's post-conditions.
let post =
    Arcs.make [ ((Yellow, GreenToYellow), 1)
                ((Red, YellowToRed), 1)
                ((Green, RedToGreen), 1) ]

/// The Petri net model representing a traffic light.
let model = Model.make pre post

/// The Petri net's initial marking.
let initialMarking =
    Marking.make [ (Red, 1) ]
