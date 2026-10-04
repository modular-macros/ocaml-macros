(* TEST
 readonly_files = "forged_mark_ppx_rewriter.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/forged_mark_ppx_rewriter.exe";
 all_modules = "forged_mark_ppx_rewriter.ml";
 ocamlc.byte;
 module = "forged_mark_ppx.ml";
 flags = "-ppx ${program}";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Nor does an attribute that a rewriter writes make a value a static
   binding. *)

let s = "hello"
let y = $(Expr.string s)
