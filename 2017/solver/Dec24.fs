module TLycken.AdventOfCode.Solutions.Dec24
open TLycken.AdventOfCode.Utils

module Bridge =
  let head = List.tryHead >> function Some (a,_) -> a | None -> 0
  let usable x (a,b) = x = a || x = b
  let align x (a,b) = if x = a then (b,a) elif x = b then (a,b) else failwithf "Tried to align (%i,%i) with %i" a b x

  let rec findBest score bridge parts =
    let bridgeHead = head bridge
    let extend p = findBest score ((align bridgeHead p) :: bridge) (Set.remove p parts)

    let usableParts = Set.filter (usable bridgeHead) parts

    if Set.isEmpty usableParts
    then score bridge
    else Set.map extend usableParts |> Set.maxElement
      


module Parse =
  let part = String.split "/" >> (function [a;b] -> Some (Parse.int a, Parse.int b) | _ -> None) >> Option.bind Option.bind2

  let components = List.choose part >> Set.ofList

module A =

  let score = List.sumBy (fun (a,b) -> a + b)
  let solve = Parse.components >> Bridge.findBest score [] >> sprintf "%i"


module B =
  let score bridge =
    let s = A.score bridge
    let l = List.length bridge
    l, s

  let solve = Parse.components >> Bridge.findBest score [] >> snd >> sprintf "%i"

let solvers = A.solve, B.solve
