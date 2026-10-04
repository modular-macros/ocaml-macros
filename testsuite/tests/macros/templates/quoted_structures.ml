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

(* A quoted structure in the body of a template *)

module type P = sig $val n : int end
module type V = sig val v : int end
$let[..] make_v (n : int) : (module V) expr =
  << (module struct let v = $(Expr.int n) + 1 end : V) >>
module T $(A : P) = struct
  module Z = (val $(make_v A.n))
end
module TA = T$(struct $let n : int = 41 end)
let () = Printf.printf "%d\n" TA.Z.v
