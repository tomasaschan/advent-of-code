namespace AoC.Dec9

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
