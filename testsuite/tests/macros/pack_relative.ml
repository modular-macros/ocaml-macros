(* TEST
 readonly_files = "pack_relative_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "pack_relative_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "pack_relative.ml";
   flags = "-I-static . -static-use pack_relative_lib.cmo";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "pack_relative_lib.cmo pack_relative.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* Package types are relative to the package's level (PACK-MATCHING.md):
   a member matches iff its serving level is the package member's
   recorded level plus the pack's ambient, which admits exactly the
   fields that exist in the block the coercion projects from.  So at
   level -1 a module of statics packs against a library's plain
   signature, a literal structure built there packs too, an unpacked
   compile-time package serves its members through the binding's
   world, and a package rebuilt from one at run time comes back
   through a run-world constructor. *)

module M = struct $let v = 41 end

(* The motivating program: a same-unit static module, packed at -1,
   consumed by the library's package-taking function. *)
$gen a () : int expr = Expr.int (Pack_relative_lib.run (module M : Pack_relative_lib.S))

(* Literal structures in compile-time code (JY's example): built in
   one block in the world of the expression, matched relatively. *)
module type S = sig val x : int end
type t = (module S)
$let pair () = ((module struct let x = 3 end) : t),
               ((module struct let x = 4 end : S))
$gen b () : int expr =
  Expr.int
    (Pack_relative_lib.run
       (let (module P : S) = fst (pair ()) in
        (module struct let v = P.x end : Pack_relative_lib.S)))

(* Unpack at -1 and read a member through the binding. *)
module type Z = sig val zero : int end
$gen c () : int expr =
  let p = (module Stdlib.Int : Z) in
  let module N = (val p) in
  Expr.int (N.zero + 5)

(* Nested modules inherit recursively. *)
module Outer = struct module Sub = struct $let w = 21 end end
$gen d () : int expr =
  Expr.int
    (Pack_relative_lib.run_nested (module Outer : Pack_relative_lib.Nested))

(* Reconstruct, don't transport: a compile-time package rebuilt at
   run time through a run-world constructor (JY's second example). *)
let mk x : t = (module struct let x = x end)
$gen rebuild (p : t) : t expr =
  let module P = (val p) in
  let n = Expr.int P.x in
  << mk $n >>
module R = (val $(rebuild (snd (pair ()))))

let () = Printf.printf "%d %d %d %d %d\n" $(a ()) $(b ()) $(c ()) $(d ()) R.x
