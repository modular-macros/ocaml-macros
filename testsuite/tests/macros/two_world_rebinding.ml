(* TEST
 readonly_files = "two_world_rebinding_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "two_world_rebinding_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I-static . -static-use two_world_rebinding_lib.cmo";
 all_modules = "two_world_rebinding.ml";
 ocamlc.byte;
 flags = "";
 all_modules = "two_world_rebinding_lib.cmo two_world_rebinding.cmo";
 program = "${test_build_directory}/two_world_rebinding.byte";
 compile_only = "false";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* A unit with a compile-time part makes a definition naming another unit in
   BOTH worlds, and both must be linkable.  An extension rebinding is where
   that is felt: a constructor is usable at either level, so the compile-time
   world needs the other unit too, and this unit must be compiled with
   -static-use and -I-static.  Without them the compile-time link fails,
   naming what it could not find -- the system asking for the second
   rebinding, not a defect (LIMITATIONS.md, two-world-rebindings). *)

macro g () = << 1 >>

exception E2 = Two_world_rebinding_lib.E

(* The compile-time incarnation is a real binding: a macro body may raise and
   match it, which is why the obligation exists. *)
macro caught () =
  (try raise (E2 4) with E2 n -> Expr.int n)

let z = $(g ())
let () = Printf.printf "%d %d\n" z $(caught ())
