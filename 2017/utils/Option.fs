namespace TLycken.AdventOfCode.Utils

module Option =
  let bind2 (a,b) =
    match a, b with
    | Some a, Some b -> Some (a, b)
    | _ -> None

  let bind3 = function
    | (Some a, Some b, Some c) -> Some (a, b, c)
    | _ -> None