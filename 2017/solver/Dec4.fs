module TLycken.AdventOfCode.Solutions.Dec4

open TLycken.AdventOfCode.Utils

let parse = List.map (splitString [|' '|])

let isValid comparer = List.countBy comparer >> List.exists (fun (_, count) -> count > 1) >> not

let solve comparer = parse >> List.filter (isValid comparer) >> List.length >> sprintf "%i"

module A =

  let comparer = id

module B =
  let sortString = asChars >> List.sort >> asString

  let comparer = sortString

let solvers = solve A.comparer, solve B.comparer