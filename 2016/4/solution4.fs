namespace AoC.Dec4

module Domain =

    open AoC.Utils.Helpers

    type Room = {
        name : string
        id : int
        checksum : string
    }

    let countLetters str =
        str
        |> asChars
        |> List.filter (fun c -> c <> '-')
        |> List.groupBy id
        |> List.map (fun (c,l) -> c, List.length l)
        
    let checksum room =
        room.name
        |> countLetters
        |> List.sortBy (fun (char, count) -> (-count, char))
        |> List.take 5
        |> List.map (fun (char, count) -> char)
        |> join
        
    let isValid room = room.checksum = checksum room

    let decrypt i c =
        match c with
        | '-' -> ' '
        | c' -> char ((((int c' + i) - int 'a') % (int 'z' - int 'a' + 1)) + (int 'a'))

    let decrypted room =
        let realName =
            room.name
            |> asChars
            |> List.map (decrypt room.id)
            |> join

        { room with name = realName }


module Parser =

    open System.Text.RegularExpressions
    open AoC.Utils.Helpers
    open Domain

    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None


    let parse str =
        let parsed =
            match str with
            | ParseRegex @"([a-z\-]+)-(\d+)\[([a-z]+)\]" [name; id; chk] -> Some (name, parseInt id, chk)
            | _ -> None

        match parsed with
        | Some (name, Some id, chk) -> Some { name = name; id = id; checksum = chk }
        | _ -> None

module Solvers =

    module A =

        open Domain
        open Parser

        let solve input =
            let result =
                input
                |> List.map parse
                |> List.choose id
                |> List.filter isValid
                |> List.sumBy (fun room -> room.id)

            sprintf "%A" result


    module B =

        open Domain
        open Parser
        open AoC.Utils.Helpers
        open System

        let solve input =
            let matching =
                input
                |> List.map parse
                |> List.choose id
                |> List.map decrypted
                |> List.filter (fun room -> (room.name).Contains "north") 

            match matching with
            | [] -> "no such room found"
            | room::_ -> sprintf "%d" room.id
