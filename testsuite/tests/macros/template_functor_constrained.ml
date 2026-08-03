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

(* Constrained applications, [module M = (F[V] : S)]: the reviewer's
   eta reduction -- (F[V] : S) = Id[F[V]] for [module Id[X:S] = X] --
   implemented directly as the two coercion applications the eta
   functor performs (fragment block and record; one coercion serves
   both through the shared layout).  Values and macros flow through the
   narrowed view; the eta form itself is also exercised. *)

module F[X : sig val b : int end] = struct
  let v = X.b * 2
  let secret = 99
  macro m () = << v + 1 >>
end
module type S = sig val v : int  macro m : unit -> int expr end

module V = struct let b = 5 end
module M = (F[V] : S)
let y = $(M.m ())

(* Composition with a literal argument. *)
module N = (F[struct let b = 7 end] : S)
let z = $(N.m ())

(* The eta form, equivalent by construction. *)
module Id[X : S] = X
module M0 = F[struct let b = 9 end]
module ME = Id[M0]
let w = $(ME.m ())

let () = Printf.printf "%d %d %d %d %d %d\n" M.v y N.v z ME.v w
