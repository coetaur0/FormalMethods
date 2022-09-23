open Core
open Parser

// The initial environment of the interpreter is empty.
let mutable env: Map<string, Value> =
    Map.empty

// The REPL (Read-Eval-Print-Loop) is executed repeatedly.
while true do
    printf ">> "
    let cmdString = System.Console.ReadLine()

    let parseResult = parse cmdString

    match parseResult with
    | Success cmd -> env <- execute cmd env
    | Failure error -> printfn $"{error}"
