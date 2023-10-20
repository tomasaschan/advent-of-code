module TLycken.AdventOfCode.Solutions.Dec13

open TLycken.AdventOfCode.Utils

module Parse =
  let layer = String.split ": " >> List.choose Parse.int >> (function [a;b] -> Some (a,b) | _ -> None)

  let layers = List.choose layer

let caught delay (depth, range) = (depth + delay) % ((range - 1) * 2) = 0

let severity (depth, range) = depth * range

module A =
  let solve = Parse.layers >> List.filter (caught 0) >> List.sumBy severity >> sprintf "%i"

module B =

  let throughAll delay = List.exists (caught delay) >> not

  let rec wait delay layers =
    if throughAll delay layers
    then delay
    else wait (delay + 1) layers

  let solve = Parse.layers >> wait 0 >> sprintf "%i"

let solvers = A.solve, B.solve
