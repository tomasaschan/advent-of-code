module TLycken.AdventOfCode.Solutions.Dec1
open TLycken.AdventOfCode.Utils

let parse = List.head >> Parse.digits

let reverseCaptcha lookahead input =
  let len = List.length input
  let pairs =
    List.mapi <| fun ix i ->
      if i = input.[(ix + lookahead) % len]
      then i
      else 0

  input
  |> pairs
  |> List.sum

let solveA = reverseCaptcha 1

let solveB input = reverseCaptcha (List.length input / 2) input

let show = sprintf "%i"

let solvers = Some (parse >> solveA >> show), Some (parse >> solveB >> show)
