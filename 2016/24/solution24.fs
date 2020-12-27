namespace AoC.Dec24

open System.Collections.Generic

module Domain =
    open AoC.Utils

    type Coord = int * int
    type Key = int

    type Terrain =
        | Wall
        | Open
        | Key of Key

    let passable =
        function
        | Wall -> false
        | Open -> true
        | Key _ -> true

    let allKeys<'a> =
        Map.toSeq
        >> Seq.map snd
        >> Seq.choose
            (function
            | Key k -> Some k
            | _ -> None)
        >> Set.ofSeq

    let initial map: Coord =
        Map.pick
            (fun k t ->
                match t with
                | Key 0 -> Some k
                | _ -> None)
            map

    module A =
        let pickup map keys c =
            match Map.find c map with
            | Wall -> (c, keys)
            | Open -> (c, keys)
            | Key k -> (c, Set.add k keys)

        let nexts map ((x, y), keys) (s: HashSet<Coord * Set<Key>>) =
            [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
            |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
            |> Seq.filter (flip Map.find map >> passable)
            |> Seq.map (pickup map keys)
            |> Seq.filter (s.Contains >> not)

        let finished all (_, keys) _ = all = keys

        let findShortestRoute map =
            let (c, _), _, n =
                SearchMut.bfs (initial map, Set.singleton 0) (nexts map) (finished (allKeys map))

            c, n

    module B =
        let pickup map allKeys keys c =
            match Map.find c map with
            | Wall -> (c, keys)
            | Open -> (c, keys)
            | Key 0 when allKeys - Set.singleton 0 = keys -> (c, Set.add 0 keys)
            | Key 0 -> (c, keys)
            | Key k when Set.contains k keys -> (c, keys)
            | Key k -> (c, Set.add k keys)

        let nexts map allKeys ((x, y), keys) (s: HashSet<Coord * Set<Key>>) =
            [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
            |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
            |> Seq.filter (flip Map.find map >> passable)
            |> Seq.map (pickup map allKeys keys)
            |> Seq.filter (s.Contains >> not)

        let finished all (_, keys) _ = keys = Set.add 0 all

        let findShortestRoute map =
            let all = allKeys map

            let _, _, n =
                SearchMut.bfs (initial map, Set.empty) (nexts map all) (finished all)

            n

module Parse =
    open AoC.Utils
    open Domain

    let terrain =
        function
        | '#' -> Some Wall
        | '.' -> Some Open
        | c -> string c |> Int.parse |> Option.map Key

    let line (y: int, l: string) =
        l.ToCharArray()
        |> List.ofArray
        |> List.enumerate
        |> List.map (fun (x, t) -> ((x, y), t))
        |> List.choose
            (fun (c, t) ->
                match terrain t with
                | Some t' -> Some(c, t')
                | None -> None)

    let map lines =
        List.enumerate lines
        |> List.collect line
        |> Map.ofList

module Solver =
    module A =
        let solve =
            Parse.map
            >> Domain.A.findShortestRoute
            >> snd
            >> sprintf "%d"

    module B =
        let solve =
            Parse.map
            >> Domain.B.findShortestRoute
            >> sprintf "%d"
