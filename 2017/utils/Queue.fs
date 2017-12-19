namespace TLycken.AdventOfCode.Utils

[<AutoOpen>]
module Collections =

  // Queue implementation from https://www.fsnotebook.net/notebook/fssnip-f7/Yet_another_immutable_queue_implementation
  let (|Created|_|) (v : Lazy<'T>) =
      if v.IsValueCreated then Some v.Value else None

  type Queue<'T> private (front : 'T list, back : 'T list) =
    let list = lazy (front @ List.rev back)

    static let Q(xs, ys) = Queue<'T>(xs,ys)

    static member OfList xs = Q(xs,[])
    static member Empty = Q([],[])

    member __.IsEmpty = front.IsEmpty && back.IsEmpty
    member __.Length = front.Length + back.Length

    member __.Enqueue x =
        match list with
        | Created value -> Q(value, [x])
        | _ -> Q(front, x :: back)

    member __.Dequeue() =
      match list with
      | Created [] -> None
      | Created (x :: xs) -> Some (x, Q(xs,[]))
      | _ ->
          match front, back with
          | [], [] -> None
          | [], _ -> Q(list.Value, []).Dequeue()
          | x::xs, ys -> Some (x, Q(xs,ys))

    member __.Peek() =
      match list with
      | Created [] -> None
      | Created (x :: _) -> Some x
      | _ ->
        match front, back with
        | [], [] -> None
        | [], _ -> Q(list.Value, []).Peek()
        | x::_, _ -> Some x

    member __.ToList() = list.Value
    override __.ToString () = list.Value.ToString()

  module Queue =
    let inline (|Q|) (q : Queue<_>) = q

    let empty<'T> = Queue<'T>.Empty

    let isEmpty (Q q) = q.IsEmpty
    let ofList ts = Queue<_>.OfList ts
    let toList (Q q) = q.ToList()
    let enqueue (Q q) x = q.Enqueue x
    let dequeue (Q q) = q.Dequeue()
    let peek (Q q) = q.Peek()
