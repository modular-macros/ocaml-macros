(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml lib/both.ml";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "modules.ml";
 compile_only = "false";
 flags = "-I-static lib -static-use counter.cmo -static-use both.cmo";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Compile-time code may bind and pack the modules of a compile-time
   library, which stand for their run-time components there. *)

$let aliased = let module C = Counter in C.next ()
$let packed =
  let m = (module Both : Both.V) in
  let module M = (val m) in
  M.v

let () = Printf.printf "%d %d\n" $(Expr.int aliased) $(Expr.int packed)
