open FParsec
open Core
open Parser

let mutable env: Map<string, Value> =
    Map.empty

while true do
    printf ">> "
    let cmdString = System.Console.ReadLine()

    let parseResult = parse cmdString

    match parseResult with
    | Success cmd -> env <- execute cmd env
    | Failure error -> printfn $"{error}"
