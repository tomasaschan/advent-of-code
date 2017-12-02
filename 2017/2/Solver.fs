module TLycken.AdventOfCode.Dec2

let checksum lineChecksum = List.sumBy lineChecksum

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
