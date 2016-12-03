namespace Dec1.Solutions

open AoC.Utils.Helpers

module A =
    type Direction = North | South | East | West
    type Instruction = Right | Left | Forward of int

    type State = {
        dir : Direction;
        x : int;
        y : int;
    }

    type History = {
        now : State;
        seen : State list;
    }

    let turnLeft dir =
        match dir with
        | North -> West
        | West -> South
        | South -> East
        | East -> North

    let turnRight dir =
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    let turn dir state =
        match dir with
        | Left -> { state with dir = turnLeft state.dir }
        | Right -> { state with dir = turnRight state.dir }
        | _ -> state

    let move steps state =
        match state with
        | { dir = East } -> { state with x = state.x + steps }
        | { dir = West } -> { state with x = state.x - steps }
        | { dir = North } -> { state with y = state.y + steps } 
        | { dir = South } -> { state with y = state.y - steps }

    let follow s i =
        match i with
        | Right -> turn Left s 
        | Left -> turn Right s
        | Forward steps -> move steps s
    
    let directionFromChar c = 
        match c with
        | 'R' -> Some Right
        | 'L' -> Some Left
        | _ -> None

    let stepcountFromChars = join >> parseInt

    let instructionFromChars cs =
        let parsed = 
            match cs with
            | d :: s -> Some (directionFromChar d, stepcountFromChars s)
            | _ -> None

        match parsed with
        | Some (Some dir, Some digits) -> [dir; Forward digits]
        | _ -> []
    let distance s = (abs s.x) + (abs s.y)


    let initState = { dir = North; x = 0; y = 0 }

    let instructions input =
        input
        |> split ", "
        |> List.map asChars
        |> List.collect instructionFromChars

    let solveA (input : string) =
        input
        |> instructions
        |> List.fold follow initState
        |> distance
        |> sprintf "%d"
