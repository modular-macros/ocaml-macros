(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* No scope extrusion: recovers from scope extrusion by resuming continuation *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m3 () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k -> Effect.Deep.continue k z

let w = $(m3 ()) 10
let () = assert (w = 10)
