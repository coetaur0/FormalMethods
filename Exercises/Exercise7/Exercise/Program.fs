open MathNet.Numerics.LinearAlgebra

let M0 =
    matrix [ [ 1.0; 1.0; 1.0 ]
             [ 2.0; 2.0; 2.0 ]
             [ 3.0; 3.0; 3.0 ] ]

let M1 =
    matrix [ [ 3.0; 2.0; 0.0 ]
             [ 1.0; 0.0; 1.0 ]
             [ 0.0; 3.0; 1.0 ] ]

printfn $"M0 + M1 = {M0 + M1}"

let v = vector [ 1.0; 2.0; 1.0 ]

printfn $"M1 * v = {M1 * v}"

printfn $"v * M1 = {v * M1}"
