module TLycken.AdventOfCode.Solutions.Dec12

open TLycken.AdventOfCode.Utils

module Parse =

  let group = function
  | Regex.Match "^(\d+) <-> (?:(\d+),? ?)+$" group -> Some group
  | _ -> None

  let groups = List.choose group >> List.map (List.choose Parse.int >> Set.ofList)


let connectedToZero alreadyConnected thisGroup =
  if Set.intersect alreadyConnected thisGroup <> Set.empty
  then Set.union alreadyConnected thisGroup
  else alreadyConnected

let onePass alreadyConnected groups = List.fold connectedToZero alreadyConnected groups

let rec connected alreadyConnected groups =
  let alreadyConnected' = onePass alreadyConnected groups
  if alreadyConnected' = alreadyConnected
  then alreadyConnected'
  else connected alreadyConnected' groups

module A =

  let solve = Parse.groups >> connected (Set.singleton 0) >> Set.count >> sprintf "%i"

module B =

  let rec partition n partitioned groups =
    let programs = Set.unionMany groups
    let unPartitioned = Set.difference programs partitioned
    let nextPartition = connected (Set.singleton (Set.minElement unPartitioned)) groups
    let partitioned' = Set.union partitioned nextPartition

    if programs = partitioned'
    then n + 1
    else partition (n + 1) partitioned' groups

  let solve = Parse.groups >> partition 0 Set.empty >> sprintf "%i"

let todo (_ : string list) = "todo"
let out<'a> = sprintf "%A"

let solvers = A.solve, B.solve