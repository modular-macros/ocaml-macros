(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* No scope extrusion: recovers from scope extrusion by discarding erroneous result *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k -> << $z >> ; << fun _ -> 0 >>

let w = $(m ()) 10
(*Eager scope extrusion check should not throw an error*)
