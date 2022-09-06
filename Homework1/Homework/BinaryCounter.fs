[<RequireQualifiedAccess>]
module BinaryCounter

open PetriNet

// ----- Binary counter --------------------------------------------------------------------------------------------- //

/// The places of the binary counter model.
type Place =
    | Start
    | Bit0
    | Bit1
    | Bit2
    | Bit0C
    | Bit1C
    | Bit2C

/// The transitions of the binary counter model.
type Transition =
    | T0
    | T1
    | T2
    | T3
    | T4

/// The pre-condition function of the binary counter model.
let pre =
    Arcs.make [ ((Start, T0), 1)
                ((Bit0C, T0), 1)

                ((Bit0, T1), 1)
                ((Bit1C, T1), 1)

                ((Bit0C, T2), 1)
                ((Bit1, T2), 1)

                ((Bit0, T3), 1)
                ((Bit1, T3), 1)
                ((Bit2C, T3), 1)

                ((Bit0, T4), 1)
                ((Bit1, T4), 1)
                ((Bit2, T4), 1) ]

/// The post-condition function of the binary counter model.
let post =
    Arcs.make [ ((T0, Bit0), 1)

                ((T1, Bit0C), 1)
                ((T1, Bit1), 1)

                ((T2, Bit0), 1)
                ((T2, Bit1), 1)

                ((T3, Start), 1)
                ((T3, Bit0C), 1)
                ((T3, Bit1C), 1)
                ((T3, Bit2), 1)

                ((T4, Start), 1)
                ((T4, Bit0C), 1)
                ((T4, Bit1C), 1)
                ((T4, Bit2C), 1) ]

/// The binary counter model.
let model = Model.make pre post

/// The initial marking of the binary counter model.
let initialMarking =
    Marking.make [ (Start, 1)
                   (Bit0C, 1)
                   (Bit1C, 1)
                   (Bit2C, 1) ]
