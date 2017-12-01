namespace TLycken.AdventOfCode.Utils

module Parse =

  let private tryParseWith parser = parser >> function
    | true, v -> Some v
    | false, _ -> None

  let int = tryParseWith System.Int32.TryParse

  let ints = 
    String.map id
    >> Seq.map (sprintf "%c")
    >> Seq.choose int
    >> List.ofSeq
