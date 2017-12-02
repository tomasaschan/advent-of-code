namespace TLycken.AdventOfCode
open TLycken.AdventOfCode.Utils

module Runner =
  let private getInstructions = function
  | [| day |] ->
      Some (day, sprintf ".\\input\\dec%s.txt" day)
  | _ ->
      None

  let private validate = function
  | Some (task, input) ->
      match (Parse.int task, System.IO.File.Exists(input)) with
      | Some task', true -> Some (task', input)
      | _ -> None
  | _ -> None

  let private getSolver solvers = function
  | Some (task, input) ->
    match Map.tryFind task solvers with
    | Some solver -> Some (solver, input)
    | _ -> None
  | _ -> None

  let private readInput = System.IO.File.ReadAllLines >> List.ofArray

  let private getInput = function
    | Some (solver, input') -> (solver, readInput input') |> Some
    | None -> None

  let private show<'a> part (solution : 'a) = printfn "%s: %A" part solution

  let private solveOne solver part = solver >> show part

  let private solve usage = function
  | Some ((A, B), input) ->
    solveOne A "A" input
    solveOne B "B" input
    0
  | _ ->
    usage ()
    1

  let run solvers usage = getInstructions >> validate >> getSolver solvers >> getInput >> solve usage
