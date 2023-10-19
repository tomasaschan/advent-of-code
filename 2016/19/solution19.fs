namespace AoC.Dec19

open AoC.Utils

open System.Collections.Generic

module Domain =

    let buildQueue elves =
        let q = Queue<int>()
        elves |> Seq.iter q.Enqueue
        q

    let playA n =
        let q = Seq.init n ((+) 1) |> buildQueue

        let rec play () =
            if q.Count = 1 then
                q.Dequeue()
            else
                let s = q.Dequeue()
                let _ = q.Dequeue()
                q.Enqueue s
                play ()

        play ()

    let playB n =
        let killers, victims =
            match n with
            | Int.Even n ->
                let ks = Seq.init (n / 2) ((+) 1) |> buildQueue

                let vs =
                    Seq.init (n / 2) ((+) (n / 2)) |> buildQueue

                ks, vs
            | n ->
                let ks = Seq.init (n / 2) ((+) 1) |> buildQueue

                let vs =
                    Seq.init (n / 2 + 1) ((+) (n / 2 + 1))
                    |> buildQueue

                ks, vs

        let rec rotate () =
            match (killers.Count, victims.Count) with
            | ks, vs when vs > ks + 1 ->
                let v = victims.Dequeue()
                killers.Enqueue v
                rotate ()
            | _ -> ()

        let rec play () =
            match (killers.Count, victims.Count) with
            | 1, 0 -> killers.Dequeue()
            | 0, 1 -> victims.Dequeue()
            | _ ->

                let killer = killers.Dequeue()
                let _ = victims.Dequeue()

                victims.Enqueue killer

                rotate ()

                play ()

        play ()


module Solver =

    open Domain

    module A =
        let solve =
            Int.parse >> Option.get >> playA >> string


    module B =
        let solve =
            Int.parse >> Option.get >> playB >> string
