(* TEST
 readonly_files = "corrupt_macro_object_lib.ml corrupt_macro_object.truncated";
 setup-ocamlc.byte-build-env;
 {
   module = "corrupt_macro_object_lib.ml";
   compile_only = "true";
   ocamlc.byte;
 }{
   (* The backslash protects the $ from ocamltest's variable expansion
      (Buffer.add_substitute); TSL strings pass backslashes through. *)
   script = "cp corrupt_macro_object.truncated \
             corrupt_macro_object_lib\$macros.cmo";
   script;
   module = "corrupt_macro_object.ml";
   compile_only = "true";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* A compile-world object shorter than the magic number -- what an
   interrupted build, a full disk or a killed compiler leaves behind.
   The reader checked the magic but not the read, so [End_of_file] escaped
   the driver's reporting entirely, as "Fatal error: exception
   End_of_file" with no location and no file name.  A corrupt marshalled
   descriptor and an out-of-range descriptor offset reached two further
   uncaught exceptions.  All of them now produce the error the wrong-magic
   case always produced, which is located and names the file. *)

let x = $(Corrupt_macro_object_lib.g ())
let () = print_int x
