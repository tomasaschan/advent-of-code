namespace AoC.Dec25

open AoC.Utils

module Solver =

    exception InvalidClockSyncException
    exception ClockSyncedException

    let init input a =
        { (input
           |> List.choose (Asembunny.parse true)
           |> Asembunny.init a 0 0 0) with
              Output = ignore }

    let test machine =
        let mutable last = 1
        let mutable n = 0

        let validate signal =
            match signal, last with
            | 1, 0 when n > 100 -> raise ClockSyncedException
            | 0, 1 when n > 100 -> raise ClockSyncedException
            | 1, 0 ->
                last <- 1
                n <- n + 1
            | 0, 1 ->
                last <- 0
                n <- n + 1
            | i, p -> raise InvalidClockSyncException

        try
            Asembunny.run { machine with Output = validate }
            |> ignore

            false
        with
        | InvalidClockSyncException -> false
        | ClockSyncedException -> true

    let solve input =
        Seq.initInfinite id
        |> Seq.filter ((init input) >> test)
        |> Seq.head
        |> sprintf "%d"
