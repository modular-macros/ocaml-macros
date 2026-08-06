(* TEST
 readonly_files = "shifted_member_selection_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "shifted_member_selection_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "shifted_member_selection.ml";
   flags = "-I-static . -static-use shifted_member_selection_lib.cmo -warn-error +76";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "shifted_member_selection_lib.cmo shifted_member_selection.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* Per-member world selection at persistent heads (the former
   one-world-per-qualified-head restriction, lifted 2026-07-30).  The
   library is dual-registered -- the working directory is run-visible
   by default and -I-static adds the compile world -- so one
   qualified head serves BOTH worlds, member by member: the library's
   macro [sm] resolves at its natural level -1 right next to the
   shifted view of its level-0 value [v], in one macro body.
   Warning 76 must NOT fire (-warn-error +76 above): nothing is
   unreachable when both worlds are registered. *)

macro m () =
  Shifted_member_selection_lib.sm
    (Expr.int (Shifted_member_selection_lib.v + 1))

(* One level down: the same selection through a nested module. *)
macro n () =
  Shifted_member_selection_lib.Inner.im
    (Expr.int (Shifted_member_selection_lib.Inner.iv
               + Shifted_member_selection_lib.v))

(* The same head keeps serving the run world at level 0. *)
let () =
  Printf.printf "member-selection: %d %d %d\n"
    $(m ()) $(n ()) Shifted_member_selection_lib.v
