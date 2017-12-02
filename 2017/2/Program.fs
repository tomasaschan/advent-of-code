open System.IO
open TLycken.AdventOfCode.Dec2
open TLycken.AdventOfCode.Utils

[<EntryPoint>]
let main argv =

    let delimiters = [|'\t'; ' '|]

    let splitString (str : string) = str.Split delimiters |> List.ofArray

    let parseLine = splitString >> List.choose Parse.int

    let parseInput = List.map parseLine

    let readInput = File.ReadAllLines >> List.ofArray

    let solveWith solver = readInput >> parseInput >> solver

    match argv with
    | [|input|] when File.Exists(input) ->
        let a = input |> solveWith A.solve
        let b = input |> solveWith B.solve
        printfn "A: %A" a
        printfn "B: %A" b
        0
    | _ ->
        printfn "Invalid input %A; expected an existing file" argv
        1
