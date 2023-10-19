namespace AoC.Dec4

module Domain =

    open AoC.Utils

    type Room = {
        name : string
        id : int
        checksum : string
    }
    let getId room = room.id

    let countLetters =
        String.asChars
        >> List.filter (fun c -> c <> '-')
        >> List.groupBy id
        >> List.map (fun (c,l) -> c, List.length l)
        
    let checksum =
        countLetters
        >> List.sortBy (fun (char, count) -> (-count, char))
        >> List.take 5
        >> List.map (fun (char, count) -> char)
        >> String.join
        
    let isValid room = room.checksum = checksum room.name

    let decryptChar i c =
        match c with
        | '-' -> ' '
        | c' -> char ((((int c' + i) - int 'a') % (int 'z' - int 'a' + 1)) + (int 'a'))

    let decrypt i = String.asChars >> List.map (decryptChar i) >> String.join

    let decrypted room =
        let realName = decrypt room.id room.name
        { room with name = realName }


module Parser =

    open AoC.Utils
    open Domain

    let parse str =
        let parsed =
            match str with
            | Regex.Match @"([a-z\-]+)-(\d+)\[([a-z]+)\]" [name; id; chk] -> Some (name, Int.parse id, chk)
            | _ -> None

        match parsed with
        | Some (name, Some id, chk) -> Some { name = name; id = id; checksum = chk }
        | _ -> None

module Solvers =

    module A =

        open Domain
        open Parser

        let solve = List.choose parse >> List.filter isValid >> List.sumBy getId >> sprintf "%d"

    module B =

        open Domain
        open Parser
        open AoC.Utils
        open System

        let containsNorth room = room.name.Contains "north"

        let solve = List.choose parse >> List.map decrypted >> List.filter containsNorth >> List.head >> getId >> sprintf "%d"