module TLycken.AdventOfCode.Solutions.Dec2

open TLycken.AdventOfCode.Utils

let checksum lineChecksum = List.sumBy lineChecksum
let parseLine = splitString [|'\t'; ' '|] >> List.choose Parse.int

let parse = List.map parseLine

module A =
  let minMax line = (List.min line, List.max line)
  let lineChecksum = minMax >> (fun (min, max) -> max - min)
  let solve = checksum lineChecksum

module B =

  let pairs list = Array.allPairs (list |> Array.ofList) (list |> Array.ofList) |> List.ofArray

  let pairSum = function
  | (a,b) when a > b && a % b = 0 -> a / b
  | _ -> 0

  let lineChecksum = pairs >> List.sumBy pairSum

  let solve = checksum lineChecksum

let show = sprintf "%i"

let solvers = Some (parse >> A.solve >> show), Some (parse >> B.solve >> show)
