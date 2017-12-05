module TLycken.AdventOfCode.Solutions.Dec5
open TLycken.AdventOfCode.Utils

let instructionSet = List.choose Parse.int >> List.mapi (fun i x -> i,x) >> Map.ofList

let inc set i = Map.add i ((Map.find i set) + 1) set

let rec move modify count i set =
  if 0 <= i && i < Map.count set
  then
    let i' = Map.find i set
    let i'' = modify i'
    let set' = Map.add i i'' set
    move modify (count+1) (i+i') set'
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