(* TEST
 readonly_files = "mixed_member_coercion_lib.ml \
                   mixed_member_coercion_inc.mli mixed_member_coercion_inc.ml \
                   mixed_member_coercion_con.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "mixed_member_coercion_lib.ml \
                  mixed_member_coercion_inc.mli mixed_member_coercion_inc.ml \
                  mixed_member_coercion_con.ml mixed_member_coercion.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "mixed_member_coercion_lib.ml \
                  mixed_member_coercion_inc.mli mixed_member_coercion_inc.ml \
                  mixed_member_coercion_con.ml mixed_member_coercion.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The siblings of dummy_member_coercion, inside a REAL macro block: a
   run submodule there is a SHAPED dummy -- a block of unit fields
   matching its layout -- so a narrowing coercion destructures it
   mechanically and yields dummy fields nothing consults.  Only
   ident-level dummies (dummy_slots) are immediates, and those the
   coercion passes through.  Both units also splice, so their own
   static programs execute the coerced compile-time blocks at build
   time; the run-world values check the coercions proper. *)

let () =
  Printf.printf "%d %d %d %d %d %d\n"
    Mixed_member_coercion_inc.Sub.y
    Mixed_member_coercion_inc.w
    Mixed_member_coercion_inc.z
    Mixed_member_coercion_con.X.Sub.y
    Mixed_member_coercion_con.X.Outer.Mid.Deep.q
    (let module R = Mixed_member_coercion_con.X.F (struct let v = 41 end) in
     R.h)
