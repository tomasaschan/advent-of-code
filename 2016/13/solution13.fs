namespace AoC.Dec13

open System.Collections.Generic
open AoC.Utils

module Solver =

    let isWall n x y =
        let mutable d =
            x * x + 3 * x + 2 * x * y + y + y * y + n

        let mutable c = 0

        while 0 < d do
            c <- c + (1 &&& d)
            d <- d >>> 1

        c % 2 = 1

    let showMap n rows cols =
        let walls =
            Seq.init rows id
            |> Seq.map (fun r ->
                Seq.init cols id
                |> Seq.map (fun c -> isWall n c r))

        let colNumbers =
            "  "
            + (Seq.init cols id
               |> Seq.map
                   ((fun x -> (x - x % 10) / 10)
                    >> string
                    >> (function
                    | "0" -> " "
                    | d -> d))
               |> Seq.reduce (+))
            + "\n  "
            + (Seq.init cols id
               |> Seq.map ((fun x -> x % 10) >> string)
               |> Seq.reduce (+))

        let rows =
            walls
            |> Seq.map
                (Seq.map (function
                    | true -> "#"
                    | false -> ".")
                 >> Seq.reduce (+))
            |> Seq.mapi (fun i r -> sprintf "%2d %s" i r)
            |> List.ofSeq
            |> String.concat "\n"

        colNumbers + "\n" + rows

    let surrounding x y =
        [ (x + 1, y)
          (x - 1, y)
          (x, y + 1)
          (x, y - 1) ]

    let penetrable n (x, y) = 0 <= x && 0 <= y && not (isWall n x y)

    let search finished n =
        let rec bfs finished q seen =
            let ((x, y), s), q' = Queue.dequeue q
            if finished (x, y) s then
                s, seen
            else
                let nexts =
                    surrounding x y
                    |> Seq.filter (penetrable n)
                    |> Seq.filter (fun p -> Set.contains p seen |> not)

                let q'' =
                    nexts
                    |> Seq.map (fun p -> (p, s + 1))
                    |> Seq.fold (fun q''' x -> Queue.enqueue x q''') q'

                let seen' =
                    nexts |> Seq.fold (fun s' x -> Set.add x s') seen

                bfs finished q'' seen'

        let seen = Set.add (1, 1) Set.empty
        let queue = Queue.singleton ((1, 1), 0)
        bfs finished queue seen


    let findPathLength n goal =
        let finished pos _ = pos = goal
        search finished n |> fst

    let findCoveredArea n l =
        let finished _ s = s >= l
        search finished n |> snd |> Set.count

    let partA input =
        match String.split "," input |> List.map Int.parse with
        | [ Some n; Some x; Some y; Some _ ] -> string <| findPathLength n (x, y)
        | _ -> "Input must be a comma-separated list of 4 ints; n,x,y,_"

    let partB input =
        match String.split "," input |> List.map Int.parse with
        | [ Some n; Some _; Some _; Some l ] -> string <| findCoveredArea n l
        | _ -> "Input must be a comma-separated list of 3 ints; n,_,_,l"
