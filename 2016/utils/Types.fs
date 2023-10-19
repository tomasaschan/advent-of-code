namespace AoC.Utils

module Types =

    type Direction = North | South | East | West

    let dirToStr d =
        match d with
        | North -> "N"
        | South -> "S"
        | East -> "E"
        | West -> "W"

    type Turn = Left | Right

    let turn d t =
        let turnLeft d' =
            match d' with
            | North -> West
            | West -> South
            | South -> East
            | East -> North
        let turnRight d' =
            match d' with
            | North -> East
            | East -> South
            | South -> West
            | West -> North

        match t with
        | Right -> turnRight d
        | Left -> turnLeft d

    type Position = { x : int; y : int }

    let move steps d p =
        match d with 
        | North -> { p with y = p.y + steps }
        | South -> { p with y = p.y - steps }
        | East -> { p with x = p.x + steps }
        | West -> { p with x = p.x - steps }

    let move1 = move 1
    