module TLycken.AdventOfCode.Solutions.Dec14

open TLycken.AdventOfCode.Utils

let read = List.head >> (fun s -> (List.init 128 (fun i -> sprintf "%s-%i" s i)))

let toBinary (i : int) = sprintf "%s" <| System.Convert.ToString(i, 2)

let asBinary = String.asChars >> List.map (sprintf "%c") >> List.choose Parse.hexInt >> List.map toBinary >> List.choose Parse.int >> List.map (sprintf "%04i") >> String.concat ""

let withCoords = List.mapi (fun i bs -> List.mapi (fun j b -> (i,j), b) bs) >> List.collect id >> Map.ofList
let createMap = List.map (Knot.hash >> asBinary >> String.asChars >> List.map ((=) '1'))
let state = read >> createMap >> withCoords

module A =
  let solve = state >> Map.toList >> List.filter snd >> List.length >> sprintf "%i"

module B =
  let inbounds1 i = 0 <= i && i <= 128
  let inbounds (i,j) = inbounds1 i && inbounds1 j
  let adjacent (i,j) = [i,j;i,j+1;i,j-1;i+1,j;i-1,j] |> List.filter inbounds

  let connected pos partitions =
    let isIn partition pos = Set.contains pos partition
    let isConnected partition = adjacent pos |> List.exists (isIn partition)
    let matching = partitions |> List.filter isConnected
    let others = partitions |> List.filter (isConnected >> not)
    matching, others

  let rec partition partitions state = function
    | [] -> List.length partitions
    | pos::queue ->
      if Map.find pos state
      then
        let matching, others = connected pos partitions
        let this = Set.unionMany matching |> Set.add pos
        let partitions' = this :: others
        partition partitions' state queue
      else
        partition partitions state queue

  let solve input =
    let s = state input
    let count = partition List.empty s (s |> Map.toList |> List.map fst)
    sprintf "%i" count

let solvers = A.solve, B.solve
