(* TEST
 readonly_files = "toplevel_statics_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "toplevel_statics_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* A static binding of a module that a template application binds in
   the toplevel.  The module's table entry is the run-time fragment,
   where a static's position holds a dummy, and a reference used to
   read it: 0, silently.  The value is in the application's record, as
   a generator's function is. *)

#load "toplevel_statics_lib.cmo";;

module P = Toplevel_statics_lib.Id1$(Toplevel_statics_lib.IntS);;

let t = $(Expr.int P.top);;
let k = $(Expr.int P.M.k);;
let v = P.M.v;;
let g = $(P.M.g << 1 >>);;

(* Through a coerced template with an alias member. *)

module Q = Toplevel_statics_lib.HI.F$(Toplevel_statics_lib.IntS);;

let k = $(Expr.int Q.M.k);;
let v = Q.M.v;;
let g = $(Q.M.g << 1 >>);;
