(* TEST
 readonly_files = "quote_module_root_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "quote_module_root_lib.ml quote_module_root.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "quote_module_root_lib.ml quote_module_root.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A quotation can reach a run-time binding without an identifier
   occurrence: through a binding operator ([let*]/[and*], named by the
   binding_op record rather than by a [Texp_ident] node), through [let
   module N = M], and through a first-class module pack.  Each names a
   run-time entity the macro must be closure-converted over; before the
   free-variable collector saw them the macro kept a free variable
   nothing supplied, and the bytecode static program dereferenced the
   dummy slot and died with SIGSEGV (native healed it by accident).

   The macros are used from ANOTHER module here, so a local name for the
   binding would be invalid: the values below can only be right if the
   capture is. *)

open Quote_module_root_lib

let a = $(letop ())
let b = $(letandop ())
let c = $(letmodule ())
let d = let module P = (val $(pack ()) : S) in P.v
let e = $(letmodule_chain ())
let f = $(both ())

(* The same forms in a macro of THIS module, whose quoted names are its
   own bindings. *)
let ( let+ ) x g = g x + 100

module L = struct let w = 7 end
module type T = sig val w : int end

macro here () = << let+ y = 1 in y >>
macro here_mod () = << let module N = L in N.w >>
macro here_pack () = << (module L : T) >>

let g = $(here ())
let h = $(here_mod ())
let i = let module P = (val $(here_pack ()) : T) in P.w

let () =
  Printf.printf "a = %d\nb = %d\nc = %d\nd = %d\ne = %d\nf = %d\n" a b c d e f;
  Printf.printf "g = %d\nh = %d\ni = %d\n" g h i
