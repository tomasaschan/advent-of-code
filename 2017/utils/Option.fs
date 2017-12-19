namespace TLycken.AdventOfCode.Utils

module Option =
  let bind2 (a,b) =
    match a, b with
    | Some a, Some b -> Some (a, b)
    | _ -> None
