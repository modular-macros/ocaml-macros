(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A constructor declared with a dollar is a constructor of
   compile-time code.  An exception or an extensible type may have
   constructors of both kinds. *)

exception $Stop of int
exception Failed of string

type t = ..
type t += $A | B | $C of int

$let stop n = raise (Stop n)
$let is_a = function A -> true | _ -> false
$let caught = try stop 1 with Stop n -> n
let failed () = try raise (Failed "f") with Failed s -> s
let b = B

(* A constructor is rebound to a constructor of the same kind. *)

exception $Halt = Stop
exception Broken = Failed
type t += $A' = A | B' = B

(* Quoted code is run-time code. *)

$let[..] guarded body = << try $body with Failed _ -> 0 >>
$let[..] carry () = try raise (Stop 2) with Stop n -> Expr.int n

module M : sig
  exception $Inner
  type t += $D
  $val inner : unit -> int
end = struct
  exception $Inner
  type t += $D
  $let inner () = try raise Inner with Inner -> 0
end

(* A constructor of the standard library, like one of the runtime, is
   a constructor of both. *)

$let lookup () = try raise Not_found with Not_found | Exit -> 0
let lookup () = try raise Not_found with Not_found | Exit -> 0
