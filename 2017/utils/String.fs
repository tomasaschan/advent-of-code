namespace TLycken.AdventOfCode.Utils

module String =
  let private joiner sep a b : string = a + sep + b
  let join sep = List.fold (joiner sep) ""