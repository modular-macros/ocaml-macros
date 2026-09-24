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

(* Under the split architecture (BODY-MACRO-LIFT.md) a body $gen
   refers to the argument's compile-time components by capturing the
   component function's parameter -- the restriction the earlier
   scheme needed is lifted by construction.  The record call in the
   $gen object re-evaluates at each load, so the capture is always of
   a live value. *)

module type S1 = sig $gen m : int expr -> int expr end

module F[X : S1] = struct
  $gen mm c = << 1 + $(X.m c) >>
end

module V = struct $gen m c = << $c * 10 >> end
module M = F[V]

let r = $(M.mm << 4 >>)
let () = print_int r; print_newline ()
