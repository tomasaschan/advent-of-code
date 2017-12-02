open TLycken.AdventOfCode.Dec2
open TLycken.AdventOfCode.Utils

[<EntryPoint>]
let main argv =

    let parseLine = splitString [|'\t'; ' '|] >> List.choose Parse.int

    let parse = List.map parseLine

    Runner.solveWith parse A.solve B.solve argv
