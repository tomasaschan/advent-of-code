module TLycken.AdventOfCode.Solutions.Dec6

open TLycken.AdventOfCode.Utils

module Memory =

  type Banks = Map<int,int>

  let ofList : int list -> Banks = List.mapi (fun i x -> (i,x)) >> Map.ofList

  let incAll amountToIncrement : Banks -> Banks = Map.map (fun _ currentValue -> currentValue + amountToIncrement)

  let private isAtMostAfter start count len index =
    if start + count < len
    then start <= index && index < start + count
    else start <= index || index < count - (len - start)

  let incSome amountToIncrement startingIndex countToIncrement (banks : Banks) : Banks =
    let len = banks |> Map.toList |> List.length

    Map.map (fun index currentValue ->
      if isAtMostAfter startingIndex countToIncrement len index
      then currentValue + amountToIncrement
      else currentValue) banks

  let zero index : Banks -> Banks = Map.map (fun i x -> if i = index then 0 else x)

  let banks : Banks -> int list = Map.toList >> List.map snd
  let largest : Banks -> int * int = Map.toList >> List.maxBy snd


  let reallocate (bs : Banks) : Banks =
    let largestBank = bs |> largest
    let count = bs |> banks |> List.length
    let toAll = snd largestBank / count
    let toSome = snd largestBank % count

    bs
    |> zero (fst largestBank)
    |> incAll toAll
    |> incSome 1 (fst largestBank + 1 % count) toSome

let parse = List.head >> String.split "\t" >> List.choose Parse.int >> Memory.ofList

module A =

  let rec findLoops seen steps banks =
    let banks' = Memory.reallocate banks
    let config = banks' |> Memory.banks
    if Set.contains config seen
    then
      steps + 1
    else
      let seen' = Set.add config seen
      let steps' = steps + 1
      findLoops seen' steps' banks'

  let solve input = parse input |> findLoops Set.empty 0 |> sprintf "%i"

module B =

  let rec findCycleLength seen steps banks =
    let banks' = Memory.reallocate banks
    let config = banks' |> Memory.banks
    let steps' = steps + 1

    match Map.tryFind config seen with
    | Some previous -> steps' - previous
    | None ->
      let seen' = Map.add config steps' seen
      findCycleLength seen' steps' banks'

  let solve input = parse input |> findCycleLength Map.empty 0 |> sprintf "%i"

let solvers = A.solve, B.solve