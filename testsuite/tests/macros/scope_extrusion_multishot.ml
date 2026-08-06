(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* No scope extrusion, but uses multi-shot continuations: quote-building
   handlers are one-shot, so the second continue raises
   Continuation_already_resumed -- reported as this splice's failure,
   not as a scope extrusion. *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k -> Effect.Deep.continue k << $z + 1 >> ; Effect.Deep.continue k << $z + 2 >>

let w = $(m ()) 10
(*Should throw an error relating to multi-shot continuations, not scope extrusion*)
