(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* No scope extrusion: recovers from scope extrusion by discarding erroneous result *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m () =
  let _ = <<fun y -> $(match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> <<0>>
  | effect R z, k -> << $z + 1 >>)>>
in << fun _ -> 0 >>

let w = $(m ()) 10

(*Best effort scope extrusion check will throw an error*)
