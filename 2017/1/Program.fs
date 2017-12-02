open TLycken.AdventOfCode.Utils
open TLycken.AdventOfCode.Dec1

[<EntryPoint>]
let main argv =

    let parse = List.head >> Parse.digits
    
    Runner.solveWith parse A.solve B.solve argv
