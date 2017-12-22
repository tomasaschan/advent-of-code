module TLycken.AdventOfCode.Tests.Dec21

open FsCheck.Xunit
open Swensen.Unquote

open TLycken.AdventOfCode.Solutions.Dec21

[<Property>]
let ``Correctly flips 2x2 horizontally`` (a : bool) b c d =
  let init = [[a; b]
              [c; d]]

  let flipped = [[b; a]
                 [d; c]]

  Fractal.flipH init =! flipped

[<Property>]
let ``Correctly flips 3x3 horizontally`` (a : bool) b c d e f g h i =
  let init = [[a; b; c]
              [d; e; f]
              [g; h; i]]

  let flipped = [[c; b; a]
                 [f; e; d]
                 [i; h; g]]

  Fractal.flipH init =! flipped

[<Property>]
let ``Correctly flips 2x2 vertically`` (a : bool) b c d =
  let init, flipped = [[a; b]
                       [c; d]], [[c; d]
                                 [a; b]]

  Fractal.flipV init =! flipped

[<Property>]
let ``Correctly flips 3x3 vertically`` (a : bool) b c d e f g h i =
  let init = [[a; b; c]
              [d; e; f]
              [g; h; i]]

  let flipped = [[g; h; i]
                 [d; e; f]
                 [a; b; c]]

  Fractal.flipV init =! flipped

[<Property>]
let ``Correctly rotates 2x2 clockwise`` (a : bool) b c d =
  let init = [[a; b]
              [c; d]]
  let rotated = [[c; a]
                 [d; b]]
  Fractal.rot init =! rotated

[<Property>]
let ``Correctly rotates 3x3 clockwise`` (a : bool) b c d e f g h i =
  let init = [[a; b; c]
              [d; e; f]
              [g; h; i]]
  let rotated = [[g; d; a]
                 [h; e; b]
                 [i; f; c]]
  Fractal.rot init =! rotated

[<Property>]
let ``Correctly identifies all permutations of 2x2`` (a : bool) b c d =
  let init = [[a; b]
              [c; d]]

  let expected = [
    [[c; a]
     [d; b]]
    [[d; c]
     [b; a]]
    [[b; d]
     [a; c]]
    [[a; b]
     [c; d]]
    [[b; a]
     [d; c]]
    [[d; b]
     [c; a]]
    [[c; d]
     [a; b]]
    [[a; c]
     [b; d]]
  ]

  let actual = Fractal.permute init |> List.ofSeq

  actual =! expected

[<Property>]
let ``Correctly divides 4x4 into 2x2s`` (a : bool) b c d e f g h i j k l m n o p =
  let init =     [[a; b; c; d]
                  [e; f; g; h]
                  [i; j; k; l]
                  [m; n; o; p]]
  let expected = [[[a; b]
                   [e; f]]
                  [[c; d]
                   [g; h]]
                  [[i; j]
                   [m; n]]
                  [[k; l]
                   [o; p]]
                 ]

  let actual = Fractal.split 2 init

  actual =! expected

[<Property>]
let ``Correctly divides 6x6 into 3x3s`` (a : bool) b c d e f g h i j k l m n o p q r s t u v w x y z å ä ö aa ab ac ad ae af ag =
  let init = [[a;  b;  c;  d;  e;  f]
              [g;  h;  i;  j;  k;  l]
              [m;  n;  o;  p;  q;  r]
              [s;  t;  u;  v;  w;  x]
              [y;  z;  å;  ä;  ö;  aa]
              [ab; ac; ad; ae; af; ag]]
  let expected =
    [
      [[a; b; c]
       [g; h; i]
       [m; n; o]]
      [[d; e; f]
       [j; k; l]
       [p; q; r]]
      [[s;  t;  u]
       [y;  z;  å]
       [ab; ac; ad]]
      [[v;  w;  x]
       [ä;  ö;  aa]
       [ae; af; ag]]
    ]

  let actual = Fractal.split 3 init

  actual =! expected

[<Property>]
let ``Splitting and unsplitting by 2 is identity`` (a : bool) b c d e f g h i j k l m n o p =
  let init = [[a; b; c; d]
              [e; f; g; h]
              [i; j; k; l]
              [m; n; o; p]]

  init |> Fractal.split 2 |> Fractal.unsplit 2 =! init

[<Property>]
let ``Splitting and unsplitting by 3 is identity`` (a : bool) b c d e f g h i j k l m n o p q r s t u v w x y z å ä ö aa ab ac ad ae af ag =
  let init = [[a;  b;  c;  d;  e;  f]
              [g;  h;  i;  j;  k;  l]
              [m;  n;  o;  p;  q;  r]
              [s;  t;  u;  v;  w;  x]
              [y;  z;  å;  ä;  ö;  aa]
              [ab; ac; ad; ae; af; ag]]

  init |> Fractal.split 3 |> Fractal.unsplit 2 =! init
