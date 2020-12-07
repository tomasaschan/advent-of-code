namespace AoC.Dec11

module Domain =

    open System.Collections.Generic
    open AoC.Utils

    type Thing =
        | Generator of string
        | Microchip of string

    type Floor =
        | First
        | Second
        | Third
        | Fourth

    let allFloors = [ First; Second; Third; Fourth ]

    let above =
        function
        | First -> Some Second
        | Second -> Some Third
        | Third -> Some Fourth
        | Fourth -> None

    let below =
        function
        | First -> None
        | Second -> Some First
        | Third -> Some Second
        | Fourth -> Some Third

    type State =
        { Things: Map<Thing, Floor>
          Elevator: Floor }

    let thingsOnFloor s f =
        s.Things
        |> Map.toSeq
        |> Seq.filter (snd >> (=) f)
        |> Seq.map fst
        // |> Seq.sortBy (function
        //     | Generator g -> (g, 0)
        //     | Microchip m -> (m, 1))
        |> List.ofSeq

    let moveThing target s thing =
        match target s.Things.[thing] with
        | Some f' ->
            // printfn "Moving %A to %A" thing f'
            Some
                { s with
                      Things = Map.change thing (fun _ -> Some f') s.Things
                      Elevator = f' }
        | None -> None

    let noGeneratorsOnFloor s f =
        thingsOnFloor s f
        |> Seq.forall (function
            | Generator _ -> false
            | Microchip _ -> true)

    let noMicrochipsOnFloor s f =
        thingsOnFloor s f
        |> Seq.forall (function
            | Generator _ -> true
            | Microchip _ -> false)

    let allMicrochipsOnFloorConnected s f =
        thingsOnFloor s f
        |> Seq.filter (function
            | Generator _ -> false
            | Microchip _ -> true)
        |> Seq.forall (function
            | Generator _ -> failwith "filtering didn't work"
            | Microchip m -> s.Things.[Generator m] = f)

    let floorOk s f =
        noGeneratorsOnFloor s f
        || noMicrochipsOnFloor s f
        || allMicrochipsOnFloorConnected s f


    let display s =
        let sb = System.Text.StringBuilder()

        let things = s.Things |> Map.toList |> List.map fst
        for f in (allFloors |> List.rev) do
            match f with
            | First -> "F1 "
            | Second -> "F2 "
            | Third -> "F3 "
            | Fourth -> "F4 "
            |> sb.Append
            |> ignore

            if f = s.Elevator then "E " else ". "
            |> sb.Append
            |> ignore

            for thing in things
                         |> Seq.sortBy (function
                             | Generator g -> (g, 0)
                             | Microchip m -> (m, 1)) do
                if f = s.Things.[thing] then
                    match thing with
                    | Generator g -> sprintf "%cG " (g.ToUpper().[0])
                    | Microchip m -> sprintf "%cM " (m.ToUpper().[0])
                else
                    sprintf ".  "
                |> sb.Append
                |> ignore
            // printf
            //     "gens: %b, chips connected: %b, OK: %b"
            //     (noGeneratorsOnFloor s f)
            //     (allMicrochipsOnFloorConnected s f)
            //     (floorOk s f)
            sb.Append "\n" |> ignore
        sb.ToString()



    let isValid (s: State) =
        let valid = allFloors |> Seq.forall (floorOk s)
        // printfn "Valid? %b" valid
        // display s
        // printfn ""

        valid

    let nextStates (s: State) =
        let moveOne dir =
            thingsOnFloor s s.Elevator
            |> Seq.choose (moveThing dir s)

        let moveTwo dir =
            let things = thingsOnFloor s s.Elevator
            Seq.allPairs things things
            |> Seq.filter (fun (a, b) -> a <> b)
            |> Seq.choose (fun (a, b) ->
                moveThing dir s a
                |> Option.bind (fun s' -> moveThing dir s' b))

        [ moveOne above
          moveOne below
          moveTwo above
          moveTwo below ]
        |> Seq.concat
        |> Seq.filter isValid

    let isDone (s: State) =
        s.Things
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.forall ((=) Fourth)

    let fastestSolution (s: State) =
        let seen = HashSet<State>()
        let mutable m = 0
        seen.Add s |> ignore

        let rec bfs q =
            match q with
            | [] -> failwith "No solution"
            | (s, steps) :: q' ->
                if isDone s then
                    steps
                else
                    let nexts =
                        nextStates s
                        |> Seq.filter (seen.Contains >> not)
                        |> Seq.map (fun s' -> (s', s' :: steps))
                        |> List.ofSeq

                    nexts
                    |> Seq.map fst
                    |> Seq.iter (seen.Add >> ignore)

                    if steps.Length > m then
                        printfn "%d moves..." steps.Length
                        m <- m + 1
                    else
                        ()

                    bfs (q' @ nexts)

        bfs [ (s, []) ]

    let exampleState =
        { Things =
              Map.ofList [ (Generator "H", Second)
                           (Microchip "H", First)
                           (Generator "L", Third)
                           (Microchip "L", First) ]
          Elevator = First }


    let solveExample _ =
        let steps = fastestSolution exampleState

        steps.Length |> string


    let parseState (input: string list) =
        let parseLine (line: string) =
            let words = line.Split(" ")

            let floor =
                match words.[1] with
                | "first" -> First
                | "second" -> Second
                | "third" -> Third
                | "fourth" -> Fourth
                | s -> failwith (sprintf "unrecognizable floor: %s" s)

            words
            |> Seq.skip 2
            |> Seq.windowed 2
            |> Seq.map List.ofArray
            |> Seq.choose (function
                | g :: "generator" :: _ -> Some <| Generator g
                | g :: "generator," :: _ -> Some <| Generator g
                | g :: "generator." :: _ -> Some <| Generator g
                | m :: "microchip" :: _ -> Some <| Microchip (m.Split "-").[0]
                | m :: "microchip," :: _ -> Some <| Microchip (m.Split "-").[0]
                | m :: "microchip." :: _ -> Some <| Microchip (m.Split "-").[0]
                | _ -> None)
            |> Seq.map (fun thing -> (thing, floor))

        input
        |> Seq.collect parseLine
        |> Map.ofSeq
        |> fun things -> { Things = things; Elevator = First }

    let solveA =
        parseState
        >> fastestSolution
        >> (fun s -> s.Length)
        >> string


    let solveB =
        parseState
        >> (fun s ->
            { s with
                  Things =
                      Seq.append
                          ([ (Generator "elerium", First)
                             (Microchip "elerium", First)
                             (Generator "dilithium", First)
                             (Microchip "dilithium", First) ]
                           |> Seq.ofList)
                          (s.Things |> Map.toSeq)
                      |> Map.ofSeq })
        >> fastestSolution
        >> (fun s -> s.Length)
        >> string
