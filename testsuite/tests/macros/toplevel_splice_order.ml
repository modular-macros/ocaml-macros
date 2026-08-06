(* TEST
 toplevel;
*)

(* Top-level splices in a toplevel phrase evaluate when the phrase's
   run-time term is constructed, in construction order, once each --
   matching batch compilation's in-place convention (3B-SCHEME.md,
   decided 2026-07-22; side_effect_intra_item pins the batch side).
   The toplevel routes each splice's thunk through the phrase's slot
   array and applies it at the hole. *)

macro noisy tag v = (Printf.printf "[ct %s]\n" tag; << $v >>)
;;

(* Two splices in one item whose term is not reordered: left to right. *)
let p = ($(noisy "l" << 1 >>), $(noisy "r" << 2 >>))
;;

(* A pattern-destructured multi-splice binding follows the term: the
   pattern-match compiler binds the tuple's components second first, so
   [b]'s splice runs before [a]'s -- construction order, not source
   order. *)
let (a, b) = ($(noisy "a" << 3 >>), $(noisy "b" << 4 >>))
;;

Printf.printf "run %d %d\n" a b
;;

(* A splice's compile-time effect runs exactly once. *)
let n = $(print_endline "[once]"; << 5 >>)
;;

(* A splice quoting a phrase-local binder. *)
macro plus1 x = << $x + 1 >>
;;

let f = fun x -> $(plus1 << x >>)
;;

f 41
;;

(* A macro defined and used across items of a single phrase: the splice
   sees the macro, and the macro body's effect runs when the thunk calls
   it at construction. *)
macro seven () = (print_endline "[seven]"; << 7 >>) let v = $(seven ())
;;

(* A quotation mentioning a run-time name of an earlier phrase: the
   returned code is closed over the name and resolved through the
   toplevel value table when the thunk runs at the hole. *)
let base = 100
;;

macro from_base () = << base + 1 >>
;;

let q = $(from_base ())
;;
