open AoC.Utils.Runner
open AoC.Dec24.Solver

[<EntryPoint>]
let main argv =

    solveFromFile A.solve B.solve argv

    0
