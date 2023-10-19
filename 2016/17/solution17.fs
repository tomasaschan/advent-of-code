namespace AoC.Dec17

open AoC.Utils

module Solve =
    let doors input x y path =
        Hash.md5 (input + path)
        |> String.asChars
        |> List.take 4
        |> List.map (function
            | 'B'
            | 'C'
            | 'D'
            | 'E'
            | 'F' -> true
            | _ -> false)
        |> function
        | [ up; dn; l; r ] -> ((1 < y && up), (y < 4 && dn), (1 < x && l), (x < 4 && r))
        | _ -> failwith "hash was too short!"

    let a input =
        let nexts input (x, y, path) _ =
            let (up, dn, l, r) = doors input x y path
            List.choose
                id
                [ if up then Some(x, y - 1, path + "U") else None
                  if dn then Some(x, y + 1, path + "D") else None
                  if l then Some(x - 1, y, path + "L") else None
                  if r then Some(x + 1, y, path + "R") else None ]
            |> Seq.ofList

        let finished (x, y, _) _ = (x, y) = (4, 4)

        let (_, _, path), _ =
            Search.bfs (1, 1, "") (nexts input) finished

        path

    let b input =
        let nexts (x, y, path) =
            if (x, y) = (4, 4) then
                []
            else
                let (up, dn, l, r) = doors input x y path
                List.choose
                    id
                    [ if up then Some(x, y - 1, path + "U") else None
                      if dn then Some(x, y + 1, path + "D") else None
                      if l then Some(x - 1, y, path + "L") else None
                      if r then Some(x + 1, y, path + "R") else None ]

        let path (_, _, p) = p
        let pos (x, y, _) = (x, y)
        Search.exhaust nexts (1, 1, "")
        |> List.filter (pos >> (=) (4, 4))
        |> List.sortByDescending (path >> String.length)
        |> List.head
        |> path
        |> String.length
        |> string
