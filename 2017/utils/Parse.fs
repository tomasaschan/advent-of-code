namespace TLycken.AdventOfCode.Utils

module Parse =

  open TLycken.AdventOfCode.Utils

  let private tryParseWith parser = parser >> function
    | true, v -> Some v
    | false, _ -> None

  let int = tryParseWith System.Int32.TryParse

  let uint = tryParseWith System.UInt32.TryParse

  let uint64 = tryParseWith System.UInt64.TryParse

  let private hexResult x offset = Operators.int x - Operators.int offset

  let private hexChar = function
    | x when '0' <= x && x <= '9' -> Some (hexResult x '0')
    | x when 'A' <= x && x <= 'F' -> Some (hexResult x 'A' + 10)
    | x when 'a' <= x && x <= 'f' -> Some (hexResult x 'a' + 10)
    | _ -> None

  let hexInt = String.asChars >> List.map hexChar >> (fun ints ->
    if List.contains None ints
    then None
    else
      let mutable res = 0 in
      for i in ints |> List.choose id do
        res <- res * 16 + i
      done
      Some res)

  let digits (s : string) = s.ToCharArray() |> Array.map (sprintf "%c") |> List.ofArray |> List.choose int
