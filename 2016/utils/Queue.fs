namespace AoC.Utils

module Queues =
    let (|Created|_|) (v: Lazy<'T>) =
        if v.IsValueCreated then Some v.Value else None

    type Queue<'T> private (front: 'T list, back: 'T list) =
        let list = lazy (front @ List.rev back)

        static let q (xs, ys) = Queue<'T>(xs, ys)

        static member OfList xs = q (xs, [])
        static member Empty = q ([], [])

        member __.IsEmpty = front.IsEmpty && back.IsEmpty
        member __.Length = front.Length + back.Length

        member __.Enqueue x =
            match list with
            | Created value -> q (value, [ x ])
            | _ -> q (front, x :: back)

        member __.Dequeue() =
            match list with
            | Created [] -> failwith "Queue underflow."
            | Created (x :: xs) -> x, q (xs, [])
            | _ ->
                match front, back with
                | [], [] -> failwith "Queue underflow."
                | [], _ -> q(list.Value, []).Dequeue()
                | x :: xs, ys -> x, q (xs, ys)

        member __.ToList() = list.Value
        override __.ToString() = list.Value.ToString()

module Queue =

    open Queues

    let inline (|Q|) (q: Queue<_>) = q
    let empty<'T> = Queue<'T>.Empty
    let ofList ts = Queue<_>.OfList ts
    let toList (Q q) = q.ToList()
    let enqueue (Q q) x = q.Enqueue x
    let dequeue (Q q) = q.Dequeue()
