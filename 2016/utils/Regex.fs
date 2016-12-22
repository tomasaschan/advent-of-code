namespace AoC.Utils  


module Regex =

    open System.Text.RegularExpressions

    let (|Match|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    let (|Matches|_|) regex str =
        let ms = Regex(regex).Matches(str)
        if ms.Count > 0
        then Some [for m in ms -> m.Value]
        else None
