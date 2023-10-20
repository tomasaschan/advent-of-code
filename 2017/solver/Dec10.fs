module TLycken.AdventOfCode.Solutions.Dec10

open TLycken.AdventOfCode.Utils

module A =
  let solve input =
    let lengths = input |> List.head |> String.split "," |> List.choose Parse.int
    let data = List.init 256 id 
    let result, _, _ = Knot.stepThrough data 0 0 lengths
    result.[0] * result.[1] |> sprintf "%i"

module B =
  let solve = List.head >> Knot.hash


let solvers = A.solve, B.solve