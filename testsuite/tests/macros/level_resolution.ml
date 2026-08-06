(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* Level-based name resolution (NAME-RESOLUTION.md Part II): the
   initial environment registers Stdlib for the compile-time world as
   well, and string-keyed (open-introduced) candidates are selected by
   the level of the use, while lexical bindings stay level-blind. *)

(* The running example: the same stdlib name at both levels.  The
   compile-time print appears in the compiler's output, not the
   program's; here we use pure functions so the program output pins the
   values. *)
macro m x = if x + 2 > 3 then << 4 + 5 >> else << 6 >>
let a = $(m 2)
let b = $(m 0)
let () = Printf.printf "plus-two-levels: %d %d\n" a b

(* Qualified projections at level -1: the module head resolves through
   the compile world's layer and the projection inherits its level. *)
macro q () = Expr.int (List.length [1;2;3] + String.length "abcd")
let () = Printf.printf "qualified: %d\n" $(q ())

(* Qualified access through a PERSISTENT head at level -1: the
   stdlib's directory is registered for both worlds, so the persistent
   entry serves the compile world with the shifted view (this used to
   be a narrowing: Stdlib.min at -1 was a level error). *)
macro qp () =
  Expr.int (Stdlib.min 1 2 + Stdlib.abs (-3) + Stdlib__List.length [1;2])
let () = Printf.printf "persistent-head: %d\n" $(qp ())

(* An open performed at level -1 opens the compile world's module. *)
macro lo () = let open List in Expr.int (length [1;2])
let () = Printf.printf "open-at-minus-one: %d\n" $(lo ())

(* The two-open example: the same unqualified name bound at level 0 by
   one open and at level -1 by another; each use selects its own
   level's candidate, and the order of the opens does not matter. *)
module M = struct let f x = x + 1 end
module N = struct macro f x = x + 2 end
open M
open N
let h x = f x + f x
macro g y = f y + f y
let () = Printf.printf "two-open: %d %d\n" (h 5) $(Expr.int (g 4))

(* Same names, opens in the other order. *)
module M2 = struct macro f2 x = x + 20 end
module N2 = struct let f2 x = x + 10 end
open M2
open N2
let h2 x = f2 x + f2 x
macro g2 y = f2 y + f2 y
let () = Printf.printf "two-open-swapped: %d %d\n" (h2 5) $(Expr.int (g2 4))

(* A macro calling a macro brought in by an open: the level -1
   candidate serves the level -1 use. *)
module Lib = struct macro double e = << $e + $e >> end
open Lib
macro user () = double << 21 >>
let () = Printf.printf "macro-via-open: %d\n" $(user ())

(* Lexical bindings win over any open candidate, regardless of level:
   shadowing structure is independent of levels. *)
let f x = x + 100  (* shadows both opens' f *)
let () = Printf.printf "local-wins: %d\n" (f 5)
