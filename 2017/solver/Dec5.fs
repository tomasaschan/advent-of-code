module TLycken.AdventOfCode.Solutions.Dec5

open System.Collections.Generic
open TLycken.AdventOfCode.Utils

let makeDict list =
  let dict = Dictionary<int, int>()
  List.iter (fun (k,v) -> dict.[k] <- v) list
  dict

let instructionSet = List.choose Parse.int >> List.mapi (fun i x -> i,x) >> makeDict

let rec move modify count i (set : IDictionary<int, int>) =
  if set.ContainsKey i
  then
    let i' = set.[i]
    let i'' = modify i'
    set.[i] <- i''
    move modify (count+1) (i+i') set
  else
    count

let solve modify = instructionSet >> move modify 0 0 >> sprintf "%i"

module A =

  let modify i = i + 1

  let solve = solve modify

module B =
  let modify i = if i >= 3 then i - 1 else i + 1

  let solve = solve modify

let solvers = Some A.solve, Some B.solve