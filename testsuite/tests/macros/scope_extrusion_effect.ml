(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* Scope extrusion with effects *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k -> << fun _ -> $z + 1 >>

let w = $(m ()) 10
