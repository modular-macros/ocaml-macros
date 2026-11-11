(* TEST
 readonly_files = "template_static_primitive_lib.ml";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "template_static_primitive_lib.ml";
 ocamlc.byte;
 all_modules = "template_static_primitive.ml";
 {
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   flags = "-I-static . -static-use template_static_primitive_lib.cmo";
   ocamlc.byte;
 }
*)

(* A template body calls an external of another unit in compile-time
   code.  Unlike a call in the body's run-time code, it is a dependency
   of the compile-time part, which must be given the other unit. *)

module T $(X : sig end) = struct
  $let n = Template_static_primitive_lib.len "abc"
  let x = $(Expr.int n)
end
module M = T$(struct end)
