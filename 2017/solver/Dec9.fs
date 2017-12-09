module TLycken.AdventOfCode.Solutions.Dec9

open TLycken.AdventOfCode.Utils
open TLycken.AdventOfCode.Utils.Debugging
open System.Security.Cryptography

type Character =
| OpenGroup
| CloseGroup
| OpenGarbage
| CloseGarbage
| Escape
| Regular

type Mode =
| Normal
| Garbage
| Escaping

module Parse =
  let character = function
  | '{' -> OpenGroup
  | '}' -> CloseGroup
  | '<' -> OpenGarbage
  | '>' -> CloseGarbage
  | '!' -> Escape
  | _ -> Regular

module A =

  let rec score total escaping garbage nestingLevel = function
    | [] -> total
    | c :: cs ->
      match Parse.character c with
      | _ when escaping -> score total false garbage nestingLevel cs
      | Escape -> score total true garbage nestingLevel cs
      | OpenGarbage when not garbage -> score total false true nestingLevel cs
      | CloseGarbage when garbage -> score total false false nestingLevel cs
      | OpenGroup when not garbage -> score total false false (nestingLevel + 1) cs
      | CloseGroup when not garbage -> score (total+nestingLevel) false false (nestingLevel-1) cs
      | _ -> score total false garbage nestingLevel cs


  let solve = List.head >> asChars >> score 0 false false 0 >> sprintf "%i"

module B =

  let rec score total escaping garbage = function
    | [] -> total
    | c :: cs ->
      match Parse.character c with
      | _ when escaping -> score total false garbage cs
      | Escape -> score total true garbage cs
      | OpenGarbage when not garbage -> score total false true cs
      | CloseGarbage when garbage -> score total false false cs
      | _ when garbage -> score (total+1) escaping garbage cs
      | _ -> score total escaping garbage cs

  let solve = List.head >> asChars >> score 0 false false >> sprintf "%i"

let solvers<'a> = A.solve, B.solve