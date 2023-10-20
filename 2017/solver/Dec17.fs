module TLycken.AdventOfCode.Solutions.Dec17

open TLycken.AdventOfCode.Utils


module A =
  let evolve state pos steps i =
    let n = List.length state
    let pos' = (pos + steps) % n

    let state' = List.insert pos' i state

    state', pos' + 1

  let rec evolveAll (state : int list) pos steps i final =
    if i = final + 1 then state.[(pos + 1) % List.length state]
    else
      let state', pos' = evolve state pos steps i
      evolveAll state' pos' steps (i+1) final
      
  let solve = List.head >> Parse.int >> function
    | Some steps -> evolveAll [0] 0 steps 1 2017 |> sprintf "%i"
    | _ -> "parsing failed"

module B =

  let rec evolve valueAfter0 pos steps i final =
    if i = final + 1 then valueAfter0
    else
      let pos' = (pos + steps) % i
      let valueAfter0' = if pos' = 0 then i else valueAfter0
      evolve valueAfter0' (pos' + 1) steps (i + 1) final

  let solve = List.head >> Parse.int >> function
    | Some steps -> evolve 1 1 steps 1 50000000 |> sprintf "%i"
    | _ -> "parsing failed"

let solvers = A.solve, B.solve