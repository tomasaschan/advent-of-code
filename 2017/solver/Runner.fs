namespace TLycken.AdventOfCode

open TLycken.AdventOfCode.Utils

module Runner =

  type Instruction =
  | One of int
  | Labelled of int * string
  | All

  let private getInstructions = function
  | [||] -> Some All
  | [| day |] ->
    match Parse.int day with
    | Some day' -> Some (One day')
    | _ -> None
  | [| day; label |] ->
    match Parse.int day with
    | Some day' -> Some (Labelled (day', label))
    | _ -> None
  | _ -> None

  let private inputForDay (i : int) = function
    | Some label -> sprintf ".\\input\\dec%i.txt.%s" i label
    | _ -> sprintf ".\\input\\dec%i.txt" i

  let private readInput label day =
    let input = inputForDay day label
    if System.IO.File.Exists(input)
    then input |> System.IO.File.ReadAllLines |> List.ofArray |> Some
    else None

  let private pickSolver solvers day = Map.tryFind day solvers

  let private gather solvers label day =
    match readInput label day, pickSolver solvers day with
    | Some input, Some (a, b) -> Some (day, label, input, a, b)
    | _ -> None

  let private show part solution time = printfn "%s: %s (%O)" part solution time

  let private solveOne solver input label =
    let stopWatch = System.Diagnostics.Stopwatch()
    stopWatch.Start()
    let solution = solver input
    stopWatch.Stop()
    show label solution stopWatch.Elapsed

  let private solveDay a b input label day =
    let labeller part = match label with Some l -> sprintf "%s (%s)" part l | None -> part
    printfn "December %i" day
    solveOne a input (labeller "A")
    solveOne b input (labeller "B")

  let runOne solvers day label =
    match gather solvers label day with
    | Some (day, label, input, a, b) ->
      solveDay a b input label day
      Some ()
    | _ -> None

  let runAll solvers =
    let days = solvers |> Map.toList |> List.map fst
    let gathered =
      days
      |> List.map (gather solvers None)
      |> List.fold (fun s -> function
        | Some i ->
          match s with
          | Some s' -> Some (i::s')
          | _ -> None
        | _ -> None) (Some [])
      |> function Some l -> Some (List.rev l) | _ -> None
    match gathered with
    | Some problems ->
      problems |> List.iter (fun (day, label, input, a, b) -> solveDay a b input label day)
      Some ()
    | _ -> None

  let private runAsInstructed solvers = function
    | One day -> runOne solvers day None
    | Labelled (day, label) -> runOne solvers day (Some label)
    | All -> runAll solvers

  let private tryRunAsInstructed solvers = function
  | Some i -> runAsInstructed solvers i
  | _ -> None

  let private report usage = function
    | Some _ -> 0
    | None -> usage (); 1

  let run solvers usage = getInstructions >> tryRunAsInstructed solvers >> report usage
