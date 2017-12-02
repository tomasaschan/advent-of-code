namespace TLycken.AdventOfCode.Utils

module Runner =
  let private readInput = System.IO.File.ReadAllLines >> List.ofArray

  let private show<'a> part (solution : 'a) = printfn "%s: %A" part solution

  let private parseArgs = function
    | [|input|] when System.IO.File.Exists(input) ->
      Some input
    | _ ->
      printfn "Invalid input; expected the path to an existing file"
      None

  let private getInput = function
    | Some input' -> readInput input' |> Some
    | None -> None

  let private solveOne parser solver part = parser >> solver >> show part

  let solveWith parser a b = parseArgs >> getInput >> function
    | Some input ->
      solveOne parser a "A" input
      solveOne parser b "B" input
      0
    | None ->
      1

  let solveBoth parseA solveA parseB solveB = parseArgs >> getInput >> function
    | Some input ->
      solveOne parseA solveA "A" input
      solveOne parseB solveB "B" input
      0
    | None ->
      1

