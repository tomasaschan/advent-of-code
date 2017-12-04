namespace TLycken.AdventOfCode

open TLycken.AdventOfCode.Utils

module Runner =
  let private show part solution = printfn "%s: %s" part solution

  let private solveOne a b input =
    match a with
    | Some a' -> a' input |> show "A"
    | _ -> ()
    match b with
    | Some b' -> b' input |> show "B"
    | _ -> ()

  let private trySolveOne = function
  | Some ((a, b), input) -> solveOne a b input |> Some
  | _ -> None

  let private inputForDay (i : int) = sprintf ".\\input\\dec%i.txt" i
  let private readInput = System.IO.File.ReadAllLines >> List.ofArray

  type Instruction =
  | One of int
  | All

  let private getInstructions = function
  | [||] -> Some All
  | [| day |] ->
    match Parse.int day with
    | Some day' -> Some (One day')
    | _ -> None
  | _ -> None

  let private validateOne day =
    let input = inputForDay day
    if System.IO.File.Exists(input)
    then Some (day, readInput input)
    else None

  let private tryPickSolver solvers = function
    | Some (day, input) ->
      match Map.tryFind day solvers with
      | Some solver -> Some (solver, input)
      | _ -> None
    | _ -> None

  let runOne solvers = validateOne >> tryPickSolver solvers >> trySolveOne

  let private pickSolver solvers (day, input) = Map.find day solvers, input

  let private validateAll<'a> =
    Map.toList
    >> List.rev
    >> List.map (fst >> validateOne)
    >> List.fold (optionMap2 (fun xs x -> x :: xs)) (Some [])

  let private pickAllSolvers solvers = function
    | Some problems ->
      problems |> List.map (pickSolver solvers) |> Some
    | _ -> None

  let runAll solvers =
    validateAll solvers
    |> pickAllSolvers solvers
    |> function
    | Some problems ->
      List.mapi (fun i ((a,b),input) ->
        printfn "December %i" (i+1)
        solveOne a b input) problems |> ignore
      Some ()
    | _ ->
      None

  let private runAsInstructed solvers = function
    | One day -> runOne solvers day
    | All -> runAll solvers

  let private tryRunAsInstructed solvers = function
  | Some i -> runAsInstructed solvers i
  | _ -> None

  let private report usage = function
    | Some _ -> 0
    | None -> usage (); 1

  let run solvers usage = getInstructions >> tryRunAsInstructed solvers >> report usage
