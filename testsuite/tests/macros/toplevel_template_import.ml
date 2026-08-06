(* TEST
 readonly_files = "toplevel_template_lib.ml";
 setup-ocamlc.byte-build-env;
 module = "toplevel_template_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I . toplevel_template_lib.cmo toplevel_template_lib\$macros.cmo";
 ocaml;
 check-ocaml-output;
*)

(* A template functor IMPORTED from a compiled unit, applied in the
   toplevel.  The component function lives in the unit's macro block
   (toplevel_template_lib$macros.cmo, loaded above alongside the
   run-time object); the toplevel-defined argument's macros -- plain
   closures in the value table -- reach it through the adapted
   compile-time view. *)

module V = struct
  let base = 10
  macro gen () = << 4 >>
end
;;

module M = Toplevel_template_lib.F[V]
;;

(M.v, M.w)
;;

(* The imported functor's instantiated macro, from a later phrase. *)
let y = $(M.m ())
;;
