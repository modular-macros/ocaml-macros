(* TEST
 readonly_files = "toplevel_import_lib.ml";
 setup-ocamlc.byte-build-env;
 module = "toplevel_import_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I . toplevel_import_lib.cmo toplevel_import_lib\$macros.cmo";
 ocaml;
 check-ocaml-output;
*)

(* A template IMPORTED from a compiled unit, applied in the
   toplevel.  The component function lives in the unit's $let[..] block
   (toplevel_import_lib$macros.cmo, loaded above alongside the
   run-time object); the toplevel-defined argument's macros -- plain
   closures in the value table -- reach it through the adapted
   compile-time view. *)

module V = struct
  let base = 10
  $let[..] gen () = << 4 >>
end
;;

module M = Toplevel_import_lib.F$(V)
;;

(M.v, M.w)
;;

(* The imported functor's instantiated $let[..], from a later phrase. *)
let y = $(M.m ())
;;
