namespace Dec1.Solutions

open AoC.Utils.Types
open AoC.Utils.Helpers

module Domain =

    type Instruction = {
        turn : Turn
        steps : int
    }

    type State = {
        here : Position
        facing : Direction
        prev : State option
    }

    let follow s i =
        let s' = { s with facing = turn s.facing i.turn }
        let s'' = { s' with here = move i.steps s'.facing s'.here }
        { s'' with prev = Some s }
        
    let rec crosses pos state =
        match state with
        | None -> false
        | Some s -> pos = s.here || crosses pos s.prev

    let rec isCrossing state =
        match state with
        | None -> false
        | Some s -> crosses s.here s.prev

    let rec hasCrossing state =
        match state with
        | None -> false
        | Some s -> isCrossing state || hasCrossing s.prev

    let isFirstCrossing state =
        isCrossing (Some state) && not (hasCrossing state.prev)

    let rec firstcrossing state =
        match state with
        | None -> None
        | Some s -> if isFirstCrossing s
                    then state
                    else firstcrossing s.prev

    let distance s = (abs s.x) + (abs s.y)

module B =
    open Domain

    let initialState = { here = { x = 0; y = 0 }; facing = North; prev = None }

    let parse input =
        let data = 
            match asChars input with
            | 'R'::digits -> (Some Right, (join >> parseInt) digits)
            | 'L'::digits -> (Some Left, (join >> parseInt) digits)
            | _ -> (None, None)

        match data with
        | (Some t, Some s) -> Some { turn = t; steps = s }
        | _ -> None

    let solveB input =
        let finalState =
            input
            |> split ", "
            |> List.map parse
            |> List.choose id
            |> List.fold follow initialState

        let x = firstcrossing (Some finalState)

        match x with
        | Some state -> sprintf "%d" (distance state.here)
        | None -> "no crossing found"        
