// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner


[<EntryPoint>]
let main argv = 
    solve argv todo todo |> printfn "%s"
    0 // return an integer exit code
