module TLycken.AdventOfCode.Solutions.Dec24
open TLycken.AdventOfCode.Utils

module Bridges =
  let usable x (_,b) = x = b
  let flip (a,b) = (b,a)

  let bridgeHead = List.tryHead >> function Some (a,_) -> a | None -> 0

  let rec findBest score bridge parts =
    let filterUsable = Set.filter (bridge |> bridgeHead |> usable)
    let extend f p = findBest score (p :: bridge) (Set.remove (f p) parts)
    let extendAll f = Set.map (extend f) >> Set.toList

    let usableParts = filterUsable parts
    let revUsableParts = parts |> Set.map flip |> Set.filter (not << Set.isIn parts) |> filterUsable

    if Set.isEmpty usableParts && Set.isEmpty revUsableParts
    then
      score bridge
    else
      (extendAll id usableParts @ extendAll flip revUsableParts)
      |> List.max
      


module Parse =
  let part = String.split "/" >> (function [a;b] -> Some (Parse.int a, Parse.int b) | _ -> None) >> Option.bind Option.bind2

  let components = List.choose part >> Set.ofList

module A =

  let score = List.sumBy (fun (a,b) -> a + b)
  let solve = Parse.components >> Bridges.findBest score [] >> sprintf "%i"


module B =
  let score bridge =
    let s = A.score bridge
    let l = List.length bridge
    l, s

  let solve = Parse.components >> Bridges.findBest score [] >> snd >> sprintf "%i"

let solvers = A.solve, B.solve
