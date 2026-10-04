(* TEST
 expect;
*)

(* Code built in the arm of a match that learns an equation, kept, and
   spliced outside the arm.  It is refused if its typing read the
   content of the equation, whatever the route: an expansion in
   unification, a label found through the refined type.  Code that only
   names the refined type is accepted. *)

type (_, _) eql = Refl : ('a, 'a) eql
type t1 = { x : int }
;;
[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
type t1 = { x : int; }
|}]

(* Unification through the equation *)
let s =
  $(let stash : type a. (a, int) eql expr -> (a -> int) expr =
      fun r ->
        let cell = ref None in
        let _ : unit expr =
          << match $r with
             | Refl -> $(cell := Some << fun (v : a) -> v + 1 >>; << () >>) >>
        in
        Option.get !cell
    in
    stash (<< assert false >> : (int, int) eql expr))
;;
[%%expect{|
Lines 2-11, characters 4-52:
 2 | ....let stash : type a. (a, int) eql expr -> (a -> int) expr =
 3 |       fun r ->
 4 |         let cell = ref None in
 5 |         let _ : unit expr =
 6 |           << match $r with
 7 |              | Refl -> $(cell := Some << fun (v : a) -> v + 1 >>; << () >>) >>
 8 |         in
 9 |         Option.get !cell
10 |     in
11 |     stash (<< assert false >> : (int, int) eql expr).
Error: The code built by this splice uses a type equation outside the match case where it holds.
|}]

(* A label found through the equation *)
let s =
  $(let stash : type a. (a, t1) eql expr -> (a -> int) expr =
      fun r ->
        let cell = ref None in
        let _ : unit expr =
          << match $r with
             | Refl -> $(cell := Some << fun (v : a) -> v.x >>; << () >>) >>
        in
        Option.get !cell
    in
    stash (<< assert false >> : (t1, t1) eql expr))
;;
[%%expect{|
Lines 2-11, characters 4-50:
 2 | ....let stash : type a. (a, t1) eql expr -> (a -> int) expr =
 3 |       fun r ->
 4 |         let cell = ref None in
 5 |         let _ : unit expr =
 6 |           << match $r with
 7 |              | Refl -> $(cell := Some << fun (v : a) -> v.x >>; << () >>) >>
 8 |         in
 9 |         Option.get !cell
10 |     in
11 |     stash (<< assert false >> : (t1, t1) eql expr).
Error: The code built by this splice uses a type equation outside the match case where it holds.
|}]

(* The refined type named, its equation not read *)
let s =
  $(let stash : type a. (a, int) eql expr -> int expr =
      fun r ->
        let cell = ref None in
        let _ : unit expr =
          << match $r with
             | Refl ->
                 $(cell := Some << let r = ref ([] : a list) in
                                   List.length !r >>;
                   << () >>) >>
        in
        Option.get !cell
    in
    stash (<< assert false >> : (int, int) eql expr))
;;
[%%expect{|
val s : int = 0
|}]

(* An argument checked against the refined type: the type checker
   expands the expected type to see whether it is a function, and then
   undoes the expansion, which is no use *)
let s =
  $(let stash : type a. (a, int) eql expr -> int expr =
      fun r ->
        let cell = ref None in
        let _ : unit expr =
          << match $r with
             | Refl ->
                 $(cell := Some << let f (y : a) (z : a) = y == z in
                                   ignore f; 1 >>;
                   << () >>) >>
        in
        Option.get !cell
    in
    stash (<< assert false >> : (int, int) eql expr))
;;
[%%expect{|
val s : int = 1
|}]
