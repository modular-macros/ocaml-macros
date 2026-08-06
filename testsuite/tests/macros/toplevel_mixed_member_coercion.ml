(* TEST
 readonly_files = "mixed_member_coercion_lib.ml \
                   mixed_member_coercion_inc.mli mixed_member_coercion_inc.ml \
                   mixed_member_coercion_con.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "mixed_member_coercion_lib.ml \
                mixed_member_coercion_inc.mli mixed_member_coercion_inc.ml \
                mixed_member_coercion_con.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* The twin route for the sibling shapes: #load runs each unit's
   macros object, whose blocks carry the narrowing coercions applied
   inside a real macro block.  Values, not just survival. *)

#load "mixed_member_coercion_lib.cmo";;

#load "mixed_member_coercion_inc.cmo";;

#load "mixed_member_coercion_con.cmo";;

Mixed_member_coercion_inc.Sub.y;;

Mixed_member_coercion_con.X.Outer.Mid.Deep.q;;

let module R = Mixed_member_coercion_con.X.F (struct let v = 41 end) in R.h;;

$(Mixed_member_coercion_con.X.m ());;
