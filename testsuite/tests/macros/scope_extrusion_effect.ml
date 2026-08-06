(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Scope extrusion with effects: the handler discards the continuation,
   carrying [<<x>>] out of its quotation, so the built code has a free
   variable.  The static program's evaluation of the splice surfaces the
   quotation check's FreeVar condition as a located error. *)
type _ Effect.t += R : int expr -> int expr Effect.t

macro m () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k -> << fun _ -> $z + 1 >>

let w = $(m ()) 10
