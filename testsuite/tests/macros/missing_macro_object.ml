(* TEST
 readonly_files = "missing_macro_object_lib.ml";
 setup-ocamlc.byte-build-env;
 {
   module = "missing_macro_object_lib.ml";
   compile_only = "true";
   ocamlc.byte;
 }{
   (* The backslash protects the $ from ocamltest's variable expansion
      (Buffer.add_substitute); TSL strings pass backslashes through. *)
   script = "rm missing_macro_object_lib\$macros.cmo";
   script;
   module = "missing_macro_object.ml";
   compile_only = "true";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* A macro of another unit lives in that unit's compile-time object, which
   the static program links; the .cmi alone is not enough.  With the .cmi
   present and l$macros.cmo absent -- a partial install, a build that
   copied only the run-time artifacts, a stale directory -- the closure
   walk finds nothing on the load path.  The error names the file it wanted
   and the unit whose macros it holds, since the file name is not one the
   user ever wrote.  The sibling failure, a file that IS there but is not
   readable, is corrupt_macro_object.ml. *)

let x = $(Missing_macro_object_lib.g ())
let () = print_int x
