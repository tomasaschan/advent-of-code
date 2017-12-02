namespace TLycken.AdventOfCode.Utils

module Parse =

  let private tryParseWith parser = parser >> function
    | true, v -> Some v
    | false, _ -> None

  let int = tryParseWith System.Int32.TryParse

  let digits (s : string) = s.ToCharArray() |> Array.map (sprintf "%c") |> List.ofArray |> List.choose int
