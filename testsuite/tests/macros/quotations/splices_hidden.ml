(* TEST
 readonly_files = "splices_hidden.mli";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "splices_hidden.mli splices_hidden.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "splices_hidden.mli splices_hidden.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The code that a splice builds may mention the components of its unit
   that the interface hides or coerces, as well as those it exports. *)

let hidden = 10
let shown = 20
external ident : int -> int = "%identity"
module M = struct let inner = 5 end
let g = $(<< hidden + shown + ident 1 + M.inner >>)
let () = print_int g; print_newline ()
