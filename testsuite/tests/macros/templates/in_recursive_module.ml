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

(* Compile-time content in the body of a recursive module.  The body
   is an expression the recursive group evaluates, so what it binds --
   the code a template application yields, a generator -- used not to
   reach the code that fills the holes of the run-time program, which
   failed with a fatal error naming the unbound variable. *)

module type S = sig type u val v : unit -> int end

module F $(X : S) : sig val get : unit -> int end =
  struct let get () = X.v () + 1 end

module V = struct type u = int let v () = 40 end

(* An application to a module outside the group. *)
module rec M1 : sig val z : unit -> int end = struct
  module A = F$(V)
  let z () = A.get ()
end

(* An application to the module itself, and to a sibling. *)
module rec M2 : sig type u = int val v : unit -> int val z : unit -> int end =
struct
  type u = int
  let v () = 20
  module A = F$(M2)
  module B = F$(N2)
  let z () = A.get () + B.get ()
end
and N2 : sig type u = int val v : unit -> int end = struct
  type u = int
  let v () = M2.v () + 1
end

(* A generator that the signature hides, called by a splice of the body. *)
module rec M3 : sig val z : unit -> int end = struct
  let k = 20
  $let[..] plus c = << $c + k >>
  let z () = $(plus << 1 >>)
end

let () = Printf.printf "%d %d %d\n" (M1.z ()) (M2.z ()) (M3.z ())
