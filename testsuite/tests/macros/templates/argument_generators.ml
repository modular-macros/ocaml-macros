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

(* A body generator refers to the argument's generators by capturing
   the component function's parameter.  The record is rebuilt at each
   load of the generator object, so the capture is always of a live
   value. *)

module type S1 = sig $val[..] m : int expr -> int expr end

module F$(X : S1) = struct
  $let[..] mm c = << 1 + $(X.m c) >>
end

module V = struct $let[..] m c = << $c * 10 >> end
module M = F$(V)

let r = $(M.mm << 4 >>)
let () = print_int r; print_newline ()
