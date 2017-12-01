module TLycken.AdventOfCode.Dec1

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

module A =
  let solve = reverseCaptcha 1

module B =
  let solve input = reverseCaptcha (List.length input / 2) input