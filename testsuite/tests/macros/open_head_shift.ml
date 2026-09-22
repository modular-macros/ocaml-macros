(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* An open serves what its head serves, within one unit.  The open
   layer's shift is the shift the head resolved under (0 for a local
   module, the compile world's layer for [List] in a generator body),
   and a name through it is then selected per member as [L.name] is.
   The refusals are in open_head_shift_errors.ml; the cross-unit,
   both-worlds case is open_both_worlds.ml. *)

(* A name quoted through an open of a same-unit run-time module
   resolves in the run world, as [<< L.s >>] does.  Previously the
   layer took the generator body's level, -1, and the level-0 use
   inside the quotation then missed it -- reported, absurdly, as
   "bound at run time, but this use is at run time". *)
module L = struct let s = 7 end
$gen g () : int expr = let open L in << s >>

(* A same-unit static module opened at the top of the unit serves its
   static at -1. *)
module St = struct $let t = 5 end
open St
$gen h () : int expr = Expr.int t

(* The compile world's Stdlib layer is what [let open List] finds in
   a generator body, so the open serves it at -1. *)
$gen k () : int expr = let open List in Expr.int (length [1; 2; 3])

(* Two opens binding the same name at two levels: each use finds the
   one at its level, in either order. *)
module A = struct let x = << 1 >> end
module B = struct $gen x () = << 2 >> end
open A
open B
let u = $(x ())
let v : int expr = x

let () = Printf.printf "%d %d %d %d\n" $(g ()) $(h ()) $(k ()) u
let () = ignore v
