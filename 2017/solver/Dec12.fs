module TLycken.AdventOfCode.Solutions.Dec12

open TLycken.AdventOfCode.Utils

module Parse =

  let group = function
  | Regex.Match "^(\d+) <-> (?:(\d+),? ?)+$" group -> Some group
  | _ -> None

  let pipe =
    function
    | h :: rest -> Parse.int h, List.map Parse.int rest
    | _ -> None, [None]
    >>
    function
    | Some h, rest -> Some (h, List.choose id rest |> Set.ofList)
    | _ -> None

  let groups = List.choose group >> List.choose pipe >> Map.ofList

let rec traverse connected visited queue groups =
  match queue with
  | [] -> connected
  | g :: rest ->
    let next = Map.find g groups
    let connected' = connected + next
    let queue' = (Set.ofList rest) + next - visited |> Set.toList
    let visited' = Set.add g visited
    traverse connected' visited' queue' groups

let partitionIncluding x = traverse Set.empty Set.empty [x]

let rec partition partitions groups =
  match Map.tryFindKey (fun g _ -> List.exists (fun g' -> Set.contains g g') partitions |> not) groups with
  | Some start ->
    let part = partitionIncluding start groups
    let partitions' = part :: partitions
    partition partitions' groups
  | None -> partitions

module A =
  let solve = Parse.groups >> partitionIncluding 0 >> Set.count >> sprintf "%i"

module B =
  let solve = Parse.groups >> partition List.empty >> List.length >> sprintf "%i"

let solvers = A.solve, B.solve