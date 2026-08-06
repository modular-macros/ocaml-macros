(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The erroneous code is discarded -- the macro's result is clean -- but
   the check is best-effort-eager: building [<< $z + 1 >>] in the
   handler, outside the scope of [z]'s binder, performs FreeVar there
   and then, so the splice's evaluation reports an extrusion even though
   the extruded code is never spliced. *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m () =
  let _ = <<fun y -> $(match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> <<0>>
  | effect R z, k -> << $z + 1 >>)>>
in << fun _ -> 0 >>

let w = $(m ()) 10

(*Best effort scope extrusion check will throw an error*)
