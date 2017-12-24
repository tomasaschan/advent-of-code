module TLycken.AdventOfCode.Solutions.Dec24
open TLycken.AdventOfCode.Utils

module Bridges =

  type Part = int * int
  type Tree = Leaf of Part | Branch of Part * Tree list

  let usable x (_,b) = x = b
  let flip (a,b) = (b,a)

  let rec build tree parts =
    let usableParts = parts |> Set.filter (tree |> fst |> usable)
    let revUsableParts = parts |> Set.map flip |> Set.filter (tree |> fst |> usable)

    if Set.isEmpty usableParts && Set.isEmpty revUsableParts
    then Leaf tree
    else
      let children = usableParts |> Set.map (fun part -> build part (Set.remove part parts)) |> Set.toList
      let revChildren = revUsableParts |> Set.map (fun part -> build part (Set.remove (flip part) parts)) |> Set.toList
      Branch (tree, children @ revChildren)


module Parse =
  let part =
    String.split "/"
    >>
    function
    | [a;b] -> Some (Parse.int a, Parse.int b)
    | _ -> None
    >>
    function
    | Some x -> Option.bind2 x : Bridges.Part option
    | _ -> None

  let components = List.choose part >> Set.ofList

module A =

  open Bridges

  let rec strongest = function
  | Leaf (a,b) -> a + b
  | Branch ((a,b), children) -> a + b + (children |> List.map strongest |> List.max)

  let solve = Parse.components >> Bridges.build (0,0) >> strongest >> sprintf "%i"


module B =
  open Bridges

  let rec lengthAndStrength l s = function
  | Leaf (a,b) -> l + 1, s + a + b
  | Branch ((a,b), children) -> children |> List.map (lengthAndStrength (l+1) (s+a+b)) |> List.max

  let solve = Parse.components >> Bridges.build (0,0) >> lengthAndStrength 0 0 >> snd >> sprintf "%i"

let solvers = A.solve, B.solve
