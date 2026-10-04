(* TEST
 subdirectories = "avail avail_run";
 setup-ocamlc.byte-build-env;
 all_modules = "avail/slib.ml avail_run/rlib.ml";
 compile_only = "true";
 ocamlc.byte;
 ocaml;
 check-ocaml-output;
*)

(* In the toplevel, as in batch compilation, the directory of a unit
   decides which code may use it; #static_use only loads its code for
   compile-time code. *)

(* A directory for compile-time code only: run-time code may not use
   the unit. *)
#static_dir "avail";;
#static_use "slib.cmo";;
$let a = Slib.u;;
let b = Slib.u;;

(* A directory for both: run-time code may use the unit, whose code must
   then be loaded for it, as that of any unit. *)
#directory "avail";;
let b = Slib.u;;
#load "slib.cmo";;
let b = Slib.u;;

(* A directory for run-time code only: compile-time code may not use the
   unit. *)
#directory "avail_run";;
#load "rlib.cmo";;
let c = Rlib.v;;
$let d = Rlib.v;;
