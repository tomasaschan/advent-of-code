open System.IO
open System.Text.RegularExpressions
open TLycken.AdventOfCode.Utils
open TLycken.AdventOfCode.Dec1


[<EntryPoint>]
let main argv =

    let input =
        match argv with
        | [| input' |] when File.Exists(input') -> File.ReadAllLines(input') |> Some
        | [| input' |] when Regex.IsMatch(input', "\\d+") -> Some [|input'|]
        | _ -> None

    let solveWith solver = List.ofArray >> List.head >> Parse.ints >> solver
    
    match input with
    | Some input' ->
        let a = input' |> solveWith A.solve
        let b = input' |> solveWith B.solve
        printfn "A: %A" a
        printfn "B: %A" b
        0
    | None ->
        printfn "Invalid input %A; expected either an existing file, or the puzzle input verbatim" argv
        1
