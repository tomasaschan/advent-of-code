namespace AoC.Dec9

module DomainOld =

    open AoC.Utils.Helpers

    let expandee len str =
        let chars = String.asChars str
        if chars.Length >= len
        then 
            let a, b = List.splitAt len chars
            String.join a, String.join b
        else "", str

    let parse str = 
        let parts =
            match str with 
            | Regex.Match "^(.*?)\((\d+)x(\d+)\)(.*)$" [prefix; length; reps; rest] ->
                Some (prefix, Int.parse length, Int.parse reps, rest)
            | _ -> None

        match parts with
        | Some (pfx, Some len, Some n, rst) -> Some (pfx, len, n, rst)
        | _ -> None

    module A =
        let rec decompressedLength str =
            match parse str with
            | Some (pfx, len, n, rst) ->
                let torepeat, suffix = expandee len rst
                Seq.sum [String.length pfx; n * String.length torepeat; decompressedLength suffix]
            | _ -> String.length str

    module B =

        let rec decompressedLength l str =
            match parse str with
            | Some (pfx, len, n, rst) ->
                decompressedLength ((n-1)*len*l) rst
            | None -> String.length str

module Domain =

    type Expander = {
        prefix : string;
        length : int;
        repetitions : int;
        target : string;
    }

    type Target =
    | E of Expander
    | S of string

    module Parse =

        open AoC.Utils.Helpers

        let expander str = 
            let parts =
                match str with 
                | Regex.Match "^(.*?)\((\d+)x(\d+)\)(.*)$" [prefix; length; reps; rest] ->
                    Some (prefix, Int.parse length, Int.parse reps, rest)
                | _ -> None

            match parts with
            | Some (pfx, Some len, Some n, rst) -> E { prefix = pfx; length = len; repetitions =  n; target = rst }
            | _ -> S str

    let splitAt count str =
        if String.length str >= count
        then str.Substring(0, count), str.Substring(count)
        else str, ""

    module A =

        let rec lengthOf t =
            match Parse.expander t with
            | S s -> String.length s
            | E x ->
                let this, rest = splitAt x.length x.target

                Seq.sum [
                    String.length x.prefix;
                    x.repetitions * String.length this;
                    lengthOf rest
                ]

    module B =
        let rec lengthOf t =
            match Parse.expander t with
            | S s -> (String.length >> bigint) s
            | E x ->
                let this, rest = splitAt x.length x.target

                Seq.sum [
                    (String.length >> bigint) x.prefix;
                    bigint x.repetitions * lengthOf this;
                    lengthOf rest
                ]

module Solver =

    module A =

        let solve = List.map (Domain.A.lengthOf >> sprintf "%d") >> String.concat "\n"
    
    module B = 

        open AoC.Utils.Helpers

        let solve = List.map (Domain.B.lengthOf >> sprintf "%A") >> String.concat "\n"