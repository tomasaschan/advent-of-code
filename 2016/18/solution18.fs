namespace AoC.Dec18

module Domain =

    type Tile = Trap | Safe

    let next = function
        | [Trap; _; Safe] -> Trap
        | [Safe; _; Trap] -> Trap
        | _ -> Safe

    let last count = List.rev >> List.take 2

    let nextRow row =
        let first = next (Safe :: List.take 2 row)
        let last = next (Safe :: (last 2 row))
        let mid = 
            row
            |> List.windowed 3
            |> List.map next
        
        List.append (first :: mid) [last]

    let private tailTip<'a> = List.rev >> List.head

    let safeTilesInRow = List.filter (fun t -> t = Safe) >> List.length

    let rec safeTiles rowCount safeCount row =
        if rowCount = 0
        then safeCount
        else
            let now = safeCount + safeTilesInRow row
            safeTiles (rowCount - 1) now (nextRow row)

module Parse =

    open Domain
    open AoC.Utils.Helpers

    let tile = function
        | '.' -> Some Safe
        | '^' -> Some Trap
        | _ -> None

    let row = AoC.Utils.Helpers.String.asChars >> List.choose tile

module Solver =

    open Domain

    module A =

        let solve rowCount = Parse.row >> (safeTiles rowCount 0) >> sprintf "%d"
