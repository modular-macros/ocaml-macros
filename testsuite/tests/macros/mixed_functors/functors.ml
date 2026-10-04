(* TEST
 readonly_files = "functors_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "functors_lib.ml functors.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "functors_lib.ml functors.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)

(* A functor whose body has compile-time components.  The static
   bindings and the functions of the generators of the body do not
   depend on the argument, and all the applications share them.  The
   code that a generator builds may mention the argument: each
   application has an environment of its own. *)

module A = Functors_lib.F (struct let v = 1 end)
module B = Functors_lib.F (struct let v = 2 end)
let a = $(A.add << 0 >>) + $(B.twice << 0 >>) + $(Expr.int A.scale)
module C = Functors_lib.Curried (struct let a = 3 end) (struct let b = 4 end)
module P = Functors_lib.Curried (struct let a = 30 end)
module D = P (struct let b = 40 end)
let c = $(C.sum ()) + $(D.sum ())
module G1 = Functors_lib.Gen ()
module G2 = Functors_lib.Gen ()
let g = $(G1.bump ()) + $(G1.bump ()) * 10 + $(G2.bump ()) * 100
module S = Functors_lib.Sealed (struct let v = 7 end)
let s = $(S.second ())
let p = $(Functors_lib.Applied.add << Functors_lib.Applied.w >>)
module H = Functors_lib.F
module E = H (struct let v = 3 end)
let e = $(E.add << 0 >>)
include Functors_lib.F (struct let v = 4 end)
let i = $(twice << w >>)
module Local (Z : sig val z : int end) = struct
  module In = Functors_lib.F (struct let v = Z.z end)
  let r = $(In.add << Z.z >>)
  $let[..] outer x = In.add << $x + Z.z >>
end
module L = Local (struct let z = 6 end)
let l = L.r + $(L.outer << 0 >>)
let () = Printf.printf "%d %d %d %d %d %d %d %d\n" a c g s p e i l
