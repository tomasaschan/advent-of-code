namespace AoC.Utils

open System

module Helpers =
    
    open System.Text.RegularExpressions

    let split (separator : string) (input : string) = 
        input.Split([| separator |], StringSplitOptions.None)
        |> List.ofArray

    let asChars (input : string) =
        input.ToCharArray()
        |> List.ofArray

    let join (s : char seq) = System.String.Concat(Array.ofSeq(s))

    let parseInt s =
        match System.Int32.TryParse(s) with
        | (true, i) -> Some i
        | (false,_) -> None 

    let rec foldk f (acc : 'State) xs =
        match xs with
        | [] -> acc
        | x::xs -> f acc x (fun acc' -> foldk f acc' xs)

    let contains y xs =
        xs 
        |> foldk (fun acc x k ->
            if   x = y
            then true
            else k acc
        ) false

    let (|RegexMatch|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    let (|RegexMatches|_|) regex str =
        let ms = Regex(regex).Matches(str)
        if ms.Count > 0
        then Some [for m in ms -> m.Value]
        else None
