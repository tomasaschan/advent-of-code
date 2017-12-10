module TLycken.AdventOfCode.Solutions.Dec10

open TLycken.AdventOfCode.Utils

let swap i j (data : int array) =
  let a, b = data.[i], data.[j]
  data.[i] <- b
  data.[j] <- a

let rec reverseSection start length (data : int array) =
  let arrLen = data.Length
  if length > 1
  then
    let j = (start + length - 1) % arrLen
    swap start j data
    reverseSection ((start + 1) % arrLen) ((length - 2) % arrLen) data

let rec stepThrough data pos skip = function
  | [] -> data, pos, skip
  | l :: ls ->
    let arr = data |> Array.ofList
    reverseSection pos l arr
    let ints' = arr |> List.ofArray
    let pos' = ((pos + l + skip) % arr.Length)
    stepThrough ints' pos' (skip+1) ls

module A =
  let solve input =
    let lengths = input |> List.head |> splitString [|','|] |> List.choose Parse.int
    let data = List.init 256 id 
    let result, _, _ = stepThrough data 0 0 lengths
    result.[0] * result.[1] |> sprintf "%i"

module B =
  let parse = List.head >> asChars >> List.map int

  let setup input =
    let appendix = [17; 31; 73; 47; 23]
    let lengths = List.append (parse input) appendix
    let ints = List.init 256 id
    lengths, ints
  
  let rec hashSparse round pos skip lengths data =
    if round = 0 then data, pos, skip
    else
      let ints', pos', skip' = stepThrough data pos skip lengths
      hashSparse (round-1) pos' skip' lengths ints'

  let hashDense = List.reduce (^^^)
    

  let solve input =
    let lengths, data = setup input
    let sparseHash, _, _ = hashSparse 64 0 0 lengths data
    sparseHash
    |> List.chunkBySize 16
    |> List.map (hashDense >> sprintf "%x")
    |> String.join ""

let solvers = A.solve, B.solve