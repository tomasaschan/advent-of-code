﻿// Learn more about F# at http://fsharp.org

open AoC.Utils.Runner
open Dec1.Solutions.A
open Dec1.Solutions.B

[<EntryPoint>]
let main argv =

    let solutions = solve1 argv solveA solveB
    
    printfn "%s" solutions
    0 // return an integer exit code
