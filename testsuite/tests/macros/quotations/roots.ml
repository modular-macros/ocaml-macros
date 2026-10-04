(* TEST
 readonly_files = "roots_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "roots_lib.ml roots.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "roots_lib.ml roots.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A quotation reaches a run-time binding through a binding operator,
   through a module that it binds or packs, and through a value of the
   module: the generator takes it in its environment. *)

open Roots_lib

let a = $(letop ())
let b = $(letandop ())
let c = $(letmodule ())
let d = let module P = (val $(pack ()) : S) in P.v
let e = $(letmodule_chain ())
let f = $(both ())

(* The same forms in a $let[..] of THIS module, whose quoted names are its
   own bindings. *)
let ( let+ ) x g = g x + 100

module L = struct let w = 7 end
module type T = sig val w : int end

$let[..] here () = << let+ y = 1 in y >>
$let[..] here_mod () = << let module N = L in N.w >>
$let[..] here_pack () = << (module L : T) >>

let g = $(here ())
let h = $(here_mod ())
let i = let module P = (val $(here_pack ()) : T) in P.w

let () =
  Printf.printf "a = %d\nb = %d\nc = %d\nd = %d\ne = %d\nf = %d\n" a b c d e f;
  Printf.printf "g = %d\nh = %d\ni = %d\n" g h i
