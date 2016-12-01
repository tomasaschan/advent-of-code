namespace AoC.Utils

module IO =

    let read = fun (filename : string) ->
        if System.IO.File.Exists filename
        then (System.IO.File.ReadLines filename)
        else [] |> List.toSeq
