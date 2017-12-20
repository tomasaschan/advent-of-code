module TLycken.AdventOfCode.Solutions.Dec19

type Direction = Up | Down | Left | Right
type Terrain = Vertical | Horizontal | Corner | Letter of string | Void

module Parse =
  open TLycken.AdventOfCode.Utils
  let location = function
    | "|" -> Some Vertical
    | "-" -> Some Horizontal
    | "+" -> Some Corner
    | Regex.Match "([A-Z])" [letter] -> Some <| Letter letter
    | " " -> Some Void
    | _ -> None

  let row = String.asChars >> List.map (sprintf "%c") >> List.choose location

  let map = List.map (row >> List.indexed) >> List.indexed >> List.collect (fun (i,r) -> List.map (fun (j,t) -> (j,i), t) r) >> Map.ofList

module Position =
  let rightOf (i,j) = i+1, j
  let leftOf (i,j) = i-1, j
  let above (i,j) = i, j-1
  let below (i,j) = i, j+1
  let nextTo pos = function
    | Up -> above pos
    | Down -> below pos
    | Left -> leftOf pos
    | Right -> rightOf pos

module Terrain =
  let get (i,j) map =
    match Map.tryFind (i,j) map with
    | Some t -> t
    | _ -> Void

  let rightOf = Position.rightOf >> get
  let leftOf = Position.leftOf >> get
  let above = Position.above >> get
  let below = Position.below >> get
  let nextTo = function
    | Up -> above
    | Down -> below
    | Left -> leftOf
    | Right -> rightOf

  let x = fst
  let y = snd
  let coord = fst

  let width<'a, 'b> = Map.toList >> List.maxBy (coord >> x) >> coord >> x
  let height<'a, 'b> = Map.toList >> List.maxBy (coord >> y) >> coord >> y

  let isInside (i,j) map =
    let w, h = width map, height map
    let inbounds l x = 0 <= x && x <= l
    inbounds w i && inbounds h j

  let print<'a> =
    let printPosition = snd >> function
      | Vertical -> "|"
      | Horizontal -> "-"
      | Corner -> "+"
      | Letter x -> x
      | Void -> " "
    let printRow = List.map printPosition >> String.concat ""
    Map.toList >> List.groupBy (coord >> y) >> List.map snd >> List.map printRow >> String.concat "\n"

module PathFinder =

  let findStart map =
    Seq.init (Terrain.width map) id
    |> Seq.filter (fun i -> Terrain.get (i,0) map <> Void)
    |> Seq.head
    |> (fun i -> i,0)

  let next map pos dir =
    match Terrain.get pos map with
    | Vertical | Horizontal | Letter _ -> Some <| (Position.nextTo pos dir, dir)
    | Corner ->
      match dir with
      | Up | Down ->
        match Terrain.rightOf pos map, Terrain.leftOf pos map with
        | Horizontal, _ | Corner, _ | Letter _, _ -> Some <| (Position.rightOf pos, Right)
        | _, Horizontal | _, Corner | _, Letter _ -> Some <| (Position.leftOf pos, Left)
        | _, _ -> failwithf "Ambiguous position %A" pos
      | Left | Right ->
        match Terrain.above pos map, Terrain.below pos map with
        | Vertical, _ | Corner, _ | Letter _, _ -> Some <| (Position.above pos, Up)
        | _, Vertical | _, Corner | _, Letter _ -> Some <| (Position.below pos, Down)
        | _, _ -> failwithf "Ambiguous position %A" pos
    | Void -> None

  let rec follow s i map pos dir =
    match next map pos dir with
    | Some (pos', dir') ->
      let s' = 
        match Terrain.get pos' map with
        | Letter l -> s + l
        | _ -> s
      follow s' (i+1) map pos' dir'
    | None -> s,i

let solve part input =
  let map = Parse.map input
  let s = PathFinder.findStart map
  PathFinder.follow "" 0 map s Down |> part

let solvers = solve fst, solve snd >> sprintf "%i"