namespace AoC.Utils.Helpers

module Int =

    let parse s =
        match System.Int32.TryParse(s) with
        | (true, i) -> Some i
        | (false,_) -> None

    let (|Even|_|) i =
        if i % 2 = 0 then Some i
        else None

    let (|Odd|_|) i =
        if i % 2 <> 0 then Some i
        else None