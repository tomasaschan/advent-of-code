namespace AoC.Utils

module Int =

    let parse (s : string) =
        match System.Int32.TryParse(s) with
        | (true, i) -> Some i
        | (false,_) -> None

    let parseBig (s : string) =
        match bigint.TryParse (s) with
        | (true, i) -> Some i
        | (false, _) -> None

    let (|Even|_|) i =
        if i % 2 = 0 then Some i
        else None

    let (|Odd|_|) i =
        if i % 2 <> 0 then Some i
        else None

    let isEven i = i % 2 = 0
    let isOdd i = i % 2 = 1
