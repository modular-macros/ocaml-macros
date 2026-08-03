(* TEST
 readonly_files = "include_chain_lib.ml include_chain_mid.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "include_chain_lib.ml include_chain_mid.ml include_chain.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "include_chain_lib.ml include_chain_mid.ml include_chain.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Using a macro of a module whose macros all come via [include] of a named
   module: the static program links the middle module's macro object, which
   pulls in the library's transitively.  [gen]'s environment is reached
   through the middle module's run-time block, which re-exports it. *)

let z = $(Include_chain_mid.gen ())
let c = $(Include_chain_mid.closed ())

let () = Printf.printf "z = %d\nc = %d\n" z c
