(* TEST
 flags = "-stop-after typing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A copy of a template under a signature is another template: its
   applications have paths of their own, and their abstract types are
   not those of the original's. *)

module type U = sig type u end
module M = struct type u = int end
module G $(X : U) : sig type s val mk : unit -> s end =
  struct type s = int let mk () = 0 end
module G' : $(X : U) -> sig type s val mk : unit -> s end = G

module A = G$(M)
module C = G'$(M)

let distinct (x : A.s) : C.s = x
