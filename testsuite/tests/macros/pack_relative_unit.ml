(* TEST
 readonly_files = "pack_relative_unit_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "pack_relative_unit_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "pack_relative_unit.ml";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "pack_relative_unit_lib.cmo pack_relative_unit.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* A static-bearing module of ANOTHER unit, packed at level -1 with no
   flags: its statics serve -1 through their own marks, so matching
   admits them, and the path must resolve to the unit's macros object
   -- which is why an expression-level module's single-block
   translation may not force the world for PATH sources: nested inside
   the unpack's own single-block region, the pack of a path still
   routes by the true world (PACK-MATCHING.md §8, single_block).
   Measured before the fix: the static program tried to link the
   unit's run-time global and failed. *)

module type S = sig val v : int val w : int end

$gen g () : int expr =
  let module N = (val (module Pack_relative_unit_lib : S)) in
  Expr.int (N.v + N.w)

(* The same pack written at the SPLICE SITE rather than in a
   generator body.  A splice's body is compile-time code, but it is
   translated from inside the run-time pass ([splice_thunk]), which
   used to leave the world alone: value references carry an address
   per world and never noticed, while a module path consults the
   world, and until packs and [let module] were admitted at -1 a
   splice body could not contain a module expression at all.  So this
   pack emitted the library's RUN global and the static program failed
   to link it, while the generator-body form above worked. *)
$gen h (module M : S) : int expr = Expr.int (M.v + M.w)

let () =
  print_int $(g ());
  print_char ' ';
  print_int $(h (module Pack_relative_unit_lib : S));
  print_newline ()
