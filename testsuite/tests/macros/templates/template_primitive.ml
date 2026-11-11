(* TEST
 readonly_files = "template_primitive_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "template_primitive_lib.ml template_primitive.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "template_primitive_lib.ml template_primitive.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A template body calls an external of another unit.  The fragment
   is quoted in the compile-time part, whose program does not link the
   other unit; the unit that expands the template calls the external. *)

module T $(X : sig end) = struct
  let f n = Template_primitive_lib.create n
end
module M = T$(struct end)

let () = Printf.printf "%d\n" (Bytes.length (M.f 3))
