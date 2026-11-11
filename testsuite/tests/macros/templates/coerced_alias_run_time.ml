(* TEST
 readonly_files = "coerced_alias_run_time_lib.ml coerced_alias_run_time_use.mli \
   coerced_alias_run_time_use.ml coerced_alias_run_time_tpl.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "coerced_alias_run_time_lib.ml coerced_alias_run_time_use.mli \
     coerced_alias_run_time_use.ml coerced_alias_run_time_tpl.ml \
     coerced_alias_run_time.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "coerced_alias_run_time_lib.ml coerced_alias_run_time_use.mli \
     coerced_alias_run_time_use.ml coerced_alias_run_time_tpl.ml \
     coerced_alias_run_time.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* An alias member, [module H = Coerced_alias_run_time_lib.B], of a
   module of another unit with no compile-time part, coerced by a
   signature: by the unit's interface beside a template application, in
   [Use], and by a template's result signature, in [Tpl].  In the
   compile-time part the coerced member is the placeholder, so the
   compile-time program does not link the other unit. *)

let () =
  Printf.printf "%d %d\n"
    Coerced_alias_run_time_use.H.x Coerced_alias_run_time_tpl.M.H.x
