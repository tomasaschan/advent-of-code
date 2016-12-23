namespace AoC.Utils

module String =

    open System

    let split (separator : string) (input : string) =
        input.Split([| separator |], StringSplitOptions.None)
        |> List.ofArray

    let asChars (input : string) =
        input.ToCharArray()
        |> List.ofArray

    let join : char seq -> string = Seq.map (sprintf "%c") >> String.concat ""

    let lowercase (s : string) = s.ToLower ()
