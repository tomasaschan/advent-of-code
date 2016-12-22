namespace AoC.Dec10

module Domain =

    open System.Collections.Generic

    type Actor =
    | Bot of int
    | Bin of int

    type Order = Lo | Hi

    type Pickup = { bot : int ; value : int }
    type Handover = { src : int; dest : Actor; order : Order }

    type Exchange =
    | Pickup of Pickup
    | Handover of Handover

    type Bot =
    | Unspecified
    | Partial of int
    | Full of int * int

    type State = { bots : Map<int, Bot>; bins : Map<int, int> }

    let bots s = s.bots
    let bins s = s.bins

    let appendValue v (bot : Bot) =
        match bot with
        | Unspecified -> Partial v
        | Partial w -> Full ((min v w), (max v w))
        | _ -> bot

    let giveToBot i v (state : State) =
        let bot =
            match Map.tryFind i state.bots with
            | Some bot -> appendValue v bot
            | None -> appendValue v Unspecified

        { state with bots = Map.add i bot state.bots }

    let giveToBin i v state = { state with bins = Map.add i v state.bins }

    let give b v =
        match b with
        | Bot b -> giveToBot b v
        | Bin b -> giveToBin b v

    let getLo bot =
        match bot with
        | Full (lo, hi) -> Some lo
        | _ -> None

    let getHi bot =
        match bot with
        | Full (lo, hi) -> Some hi
        | _ -> None

    let get bot order state =
        match Map.tryFind bot state.bots, order with
        | Some b, Lo -> getLo b
        | Some b, Hi -> getHi b
        | _ -> None

    let apply exchange state =
        match exchange with
        | Pickup p -> (give (Bot p.bot) p.value state), true
        | Handover h ->
            match (get h.src h.order state) with
            | Some v -> (give h.dest v state), true
            | None -> state, false

    let sorted = List.sortBy <| fun xch ->
        match xch with
        | Pickup p -> 0, p.value
        | Handover h -> 1, h.src

    let rec steadyState state = function
        | [] -> state
        | x::xs ->
            match apply x state with
            | s, true -> steadyState s xs
            | s, false -> steadyState s (List.append xs [x])

    let finalState = steadyState { bots = Map.empty<int,Bot> ; bins = Map.empty<int,int> }

module Parse =

    open Domain
    open AoC.Utils.Helpers

    let pickup value bot =
        match Int.parse value, Int.parse bot with
        | (Some v, Some b) -> Some { value = v; bot = b }
        | _ -> None

    let actor = function
        | Regex.Match "bot (\d+)" [bot] ->
            match Int.parse bot with
            | Some bot' -> Some (Bot bot')
            | _ -> None
        | Regex.Match "output (\d+)" [bin] ->
            match Int.parse bin with
            | Some bin' -> Some (Bin bin')
            | _ -> None
        | _ -> None

    let handover a b d =
        match a, b with
        | Some a', Some b' -> Some { src = a'; dest = b'; order = d }
        | _ -> None

    let handovers from lo hi =
        [
            handover (Int.parse from) (actor lo) Lo;
            handover (Int.parse from) (actor hi) Hi
        ]
        |> (List.choose id >> List.map Handover)

    let exchanges = List.collect <| function
        | Regex.Match "^bot (.+?) gives low to (.+?) and high to (.+?)$" [a;b;c] -> handovers a b c
        | Regex.Match "^value (\d+) goes to bot (.+?)$" [v; b] ->
            match pickup v b with
            | Some p -> [Pickup p]
            | _ -> []
        | _ -> []

module Solver =

    open Domain

    module A =

        let findTheBot = Map.pick <| fun i bot ->
            match bot with
            | Full (17, 61) -> Some i
            | _ -> None

        let solve = Parse.exchanges >> finalState >> bots >> findTheBot >> sprintf "%d"

    module B =

        let product (bins : Map<int,int>) = bins.[0] * bins.[1] * bins.[2]

        let solve = Parse.exchanges >> finalState >> bins >> product >> sprintf "%d"
