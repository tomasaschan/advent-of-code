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

    let distance s = (abs s.x) + (abs s.y)

    module Tracing =

        let move1 s =
            {
                facing = s.facing;
                here = move 1 s.facing s.here;
                prev = Some s
            }

        let rec move steps s =
            match steps with
            | 0 -> s
            | more -> move (more - 1) (move1 s)
        
        let follow s i =
            let s' = { s with facing = turn s.facing i.turn }
            { move i.steps s' with prev = Some s }

    module Crossing =
        let rec crosses pos state =
            match state with
            | None -> false
            | Some s -> pos = s.here || crosses pos s.prev

        let rec isCrossing state =
            match state with
            | None -> false
            | Some s -> crosses s.here s.prev


        let rec firstCrossing state =
            match state.prev with
            | None -> None
            | Some s ->
                match firstCrossing s with
                | None ->
                    if isCrossing (Some s)
                    then Some state
                    else None
                | Some s -> Some s


    // let rec hasCrossing state =
    //     match state with
    //     | None -> false
    //     | Some s -> isCrossing state || hasCrossing s.prev

    // let isFirstCrossing state =
    //     isCrossing (Some state) && not (hasCrossing state.prev)

    // let rec firstcrossing state =
    //     match state with
    //     | None -> None
    //     | Some s -> if isFirstCrossing s
    //                 then state
    //                 else firstcrossing s.prev

module B =
    open Domain
    open Domain.Tracing
    open Domain.Crossing

    let initialState = {
        here = { x = 0; y = 0 };
        facing = North;
        prev = None
    }

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

        let x = firstCrossing finalState

        match x with
        | Some state -> sprintf "%d" (distance state.here)
        | None -> "no crossing found"
