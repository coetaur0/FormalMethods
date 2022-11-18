open MathNet.Numerics.LinearAlgebra

// ----- Exercise 1 ------------------------------------------------------------------------------------------------- //

printfn "Exercise 1:"

let input =
    matrix [ [ 1.0; 0.0; 0.0; 0.0 ]
             [ 0.0; 1.0; 0.0; 0.0 ]
             [ 0.0; 0.0; 1.0; 0.0 ]
             [ 0.0; 0.0; 0.0; 1.0 ]
             [ 1.0; 0.0; 1.0; 0.0 ]
             [ 0.0; 0.0; 1.0; 0.0 ]
             [ 1.0; 0.0; 0.0; 0.0 ] ]

let output =
    matrix [ [ 0.0; 1.0; 0.0; 0.0 ]
             [ 1.0; 0.0; 0.0; 0.0 ]
             [ 0.0; 0.0; 0.0; 1.0 ]
             [ 0.0; 0.0; 1.0; 0.0 ]
             [ 0.0; 1.0; 0.0; 1.0 ]
             [ 1.0; 0.0; 0.0; 0.0 ]
             [ 0.0; 0.0; 1.0; 0.0 ] ]

let incidence = output - input

printfn $"Incidence matrix: {incidence}"

let initialMarking =
    vector [ 1.0
             0.0
             1.0
             0.0
             1.0
             0.0
             3.0 ]

let s0 = vector [ 3.0; 3.0; 1.0; 1.0 ]

printfn $"Marking after firing s0 = {initialMarking + incidence * s0}"

let s1 = vector [ 1.0; 1.0; 2.0; 1.0 ]

printfn $"Marking after firing s1 = {initialMarking + incidence * s1}"

// ----- Exercise 2 ------------------------------------------------------------------------------------------------- //

printfn "\nExercise 2:"

let input2 =
    matrix [ [ 0.0; 0.0; 1.0; 0.0; 0.0 ]
             [ 0.0; 0.0; 0.0; 1.0; 0.0 ]
             [ 0.0; 0.0; 1.0; 0.0; 0.0 ]
             [ 0.0; 0.0; 0.0; 1.0; 0.0 ]
             [ 0.0; 0.0; 0.0; 0.0; 18.0 ] ]

let output2 =
    matrix [ [ 0.0; 0.0; 0.0; 1.0; 0.0 ]
             [ 0.0; 0.0; 1.0; 0.0; 0.0 ]
             [ 1.0; 0.0; 0.0; 0.0; 0.0 ]
             [ 0.0; 1.0; 0.0; 0.0; 0.0 ]
             [ 0.0; 0.0; 6.0; 9.0; 0.0 ] ]

let incidence2 = output2 - input2

printfn $"Incidence matrix: {incidence2}"

let initialMarking2 =
    vector [ 1.0; 0.0; 0.0; 0.0; 0.0 ]

let s2 = vector [ 3.0; 2.0; 3.0; 2.0; 2.0 ]

printfn $"Marking after firing s2 = {initialMarking2 + incidence2 * s2}"
