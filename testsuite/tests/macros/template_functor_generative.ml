(* TEST
 flags = "-stop-after typing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Template functors are generative with respect to abstract types: two
   applications of the same template functor to the same argument yield
   unrelated abstract components.  Compare
   template_functor_accepted.ml, where the manifest component t is
   equal to int in both applications. *)

module type U = sig type u end
module M = struct type u = int end
module G[X : U] = struct type t = X.u  type s end

module A = G[M]
module B = G[M]

let generative (x : A.s) : B.s = x
