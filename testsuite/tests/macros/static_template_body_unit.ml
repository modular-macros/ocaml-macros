(* TEST
 readonly_files = "static_template_body_unit_lib.ml static_template_body_unit_lib.mli";
 setup-ocamlc.byte-build-env;
 all_modules = "static_template_body_unit_lib.mli static_template_body_unit_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "static_template_body_unit.ml";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "static_template_body_unit_lib.cmo static_template_body_unit.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* A template functor with a static in its body, defined in another
   unit and declared there with [$val s] in its result type: applied
   here, the static is evaluated in this unit's static program and its
   value is readable through the application. *)

module A = Static_template_body_unit_lib.F [struct $let v = 41 end]

let () = Printf.printf "%d %d\n" A.a $(Expr.int A.s)
