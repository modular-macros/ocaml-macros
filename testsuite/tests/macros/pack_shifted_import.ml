(* TEST
 readonly_files = "pack_shifted_import_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "pack_shifted_import_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "pack_shifted_import.ml";
   flags = "-I-static . -static-use pack_shifted_import_lib.cmo";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "pack_shifted_import_lib.cmo pack_shifted_import.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* First-class modules over shifted imports, in compile-time code: a
   module served at compile time (the dual-registered library, or the
   stdlib) can be packed at level -1 and consumed by the library's own
   package-taking function; a package of types alone is inert and may
   name any module.  The refused shapes -- packing a module this world
   holds only as a placeholder -- are pinned by pack_compile_time.ml;
   this test pins the shapes that must keep working. *)

module type Z = sig val zero : int end
module type TO = sig type t end
module Local = struct type t = int let v = 5 end

$gen m () : int expr =
  Expr.int
    (Pack_shifted_import_lib.consume
       (module Pack_shifted_import_lib.V : Pack_shifted_import_lib.S))

$gen n () : int expr =
  let _z = (module Stdlib.Int : Z) in
  let _t = (module Local : TO) in
  << 8 >>

let () = Printf.printf "%d %d\n" $(m ()) $(n ())
