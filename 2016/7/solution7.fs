namespace AoC.Dec7

module Domain =

    open AoC.Utils

    let windowed size = Seq.windowed size >> Seq.map List.ofSeq

    let isAbba chars =
        match chars with
        | [a;b;c;d] -> a = d && b = c && a <> b
        | _ -> false

    let hasAbba = String.asChars >> windowed 4 >> Seq.exists isAbba

    let supportsTLS (hnets, others) = Seq.exists hasAbba others && not (Seq.exists hasAbba hnets)

    let isAba chars =
        match chars with
        | [a;b;c] -> a = c && a <> b
        | _ -> false

    let abas = String.asChars >> windowed 3 >> Seq.filter isAba

    let corresponds aba bab =
        match aba, bab with
        | [a;b;c],[d;e;f] when a = c && a = e && a <> b && b = d && b = f -> true
        | _ -> false

    let isBab aba chars =
        match chars with
        | [a;b;c] when corresponds aba [a;b;c] -> true
        | _ -> false

    let hasBab aba = String.asChars >> windowed 3 >> Seq.exists (isBab aba)

    let hasBabInHnets hnets aba = hnets |> Seq.exists (hasBab aba)

    let supportsSSL (hnets, others) =
        others
        |> Seq.collect abas
        |> Seq.exists (hasBabInHnets hnets)

module Parse =

    open AoC.Utils

    let hypernetPattern = "\[(\w+)\]"

    let private _hypernets str =
        match str with
        | Regex.Matches hypernetPattern matches -> Some matches
        | _ -> None

    let address str =
        match _hypernets str with
        | Some parts -> parts, System.Text.RegularExpressions.Regex.Replace(str, hypernetPattern, "|") |> String.split "|"
        | None -> [], [str]

module Solvers =

    open Domain

    module A =

        let solve = List.map Parse.address >> Seq.filter supportsTLS >> Seq.length >> sprintf "%d"

    module B =
        let solve = List.map Parse.address >> Seq.filter supportsSSL >> Seq.length >> sprintf "%d"
