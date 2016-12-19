namespace AoC.Utils.Helpers

module Seq =

    let rec foldk f (acc : 'State) xs =
        match xs with
        | [] -> acc
        | x::xs -> f acc x (fun acc' -> foldk f acc' xs)

    let contains y xs =
        xs
        |> foldk (fun acc x k ->
            if   x = y
            then true
            else k acc
        ) false

    let mapReduce mapper reducer = Seq.map mapper >> Seq.reduce reducer

    let prepend a s =
        seq {
            yield a
            yield! s
        }

    let skipOrEmpty c (s : 'a seq) =
        seq {
            use e = s.GetEnumerator()
            let idx = ref 0
            let loop = ref true

            while !idx < c && !loop do
                if not (e.MoveNext())
                then loop := false

                idx := !idx + 1
            
            while e.MoveNext() do
                yield e.Current
        }
