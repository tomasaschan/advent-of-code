// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner
open AoC.Dec21.Solver

[<EntryPoint>]
let main argv = 

    let apass, bpass, args =
        Array.head argv,
        Array.head (Array.tail argv),
        Array.tail (Array.tail argv)

    solveFromFile (A.solve apass) (B.solve bpass)  args

    0 // return an integer exit code
