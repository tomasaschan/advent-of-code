namespace AoC.Dec15

module Theory =

    type Congruence = { a : int ; n : int } // x ≡ a (mod n)
    let congruence a n = { a = a ; n = n }
    let congruents c = Seq.initInfinite (fun i -> c.a + i * c.n)

    let private _add (x : int) (y : int) = (x + y)
    let private _congruent n a = a % n = 0

    let private sieveOne c =
        congruents
        >> Seq.map (_add c.a)
        >> Seq.filter (_congruent c.n)
        >> Seq.head

    let rec private _sieve c = function
        | [] -> c.a
        | head::tail -> _sieve { head with a = sieveOne { a = c.a ; n = c.n * head.n } head ; n = c.n * head.n } tail

    let sieve = function
        | [] -> 0
        | head::tail -> _sieve head tail

module Domain =

    open Theory

    type Disc = { slots : int; position : int }

    let disc slots position = { slots = slots ; position = position }
    let slots d = d.slots
    let position d = d.position

    let congruence d = {
        a = slots d - position d
        n = slots d
    }
    let congruences = List.map congruence

    let releaseTime =
        List.sortByDescending slots
        >> congruences
        >> Theory.sieve

module Parse =

    open AoC.Utils.Helpers
    open Domain

    let private parts = function
        | Regex.Match "^Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).$" [i; tot; now] -> Some (List.map Int.parse [i; tot; now])
        | _ -> None

    let private _disc str =
        match parts str with
        | Some [Some i; Some tot; Some now] -> Some (disc tot now)
        | _ -> None

    let discs = List.choose _disc

module Solver =
    open Domain

    module A =

        open AoC.Utils

        let solve = Parse.discs >> releaseTime >> sprintf "%d"
