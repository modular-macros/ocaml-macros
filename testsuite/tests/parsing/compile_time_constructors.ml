(* TEST
 flags = "-dsource -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Compile-time constructors.  This test only exercises the syntax.
   The dollar is glued to the name of the constructor. *)

exception $A
exception $B of int * string
exception $C = A
exception $D : int -> exn

type t = ..
type t += $E | F | $G of int
type t += $H = E

module type S = sig
  exception $A
  exception $B of int
  type t += $E | F
end
