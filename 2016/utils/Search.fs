namespace AoC.Utils

module Search =

    let rec private search finished nexts put get cont seen =
        let cur, cont' = get cont

        if finished cur seen then
            cur, seen
        else
            let nexts' = nexts cur seen

            let cont'' = nexts' |> Seq.fold put cont'

            let seen' =
                nexts' |> Seq.fold (fun s x -> Set.add x s) seen

            search finished nexts put get cont'' seen'

    let bfs (start: 'a) (nexts: 'a -> Set<'a> -> 'a seq) (finished: 'a -> Set<'a> -> bool) =
        let q = Queue.singleton start
        let s = Set.add start Set.empty
        search finished nexts (fun q' x -> Queue.enqueue x q') Queue.dequeue q s

    let rec exhaust (nexts: 'a -> 'a list) (cur: 'a) =
        match nexts cur with
        | [] -> [ cur ]
        | nexts' -> nexts' |> List.collect (exhaust nexts)
