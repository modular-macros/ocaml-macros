(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Mixed functors, batch bytecode (SPLITTING-FUNCTORS.md stage 2):
   the compile side is the argument-blind shared block, the run side
   an ordinary functor whose result carries per-application
   environment slots.  Shapes: definition, application, projection
   and splicing; distinct env values per application; sibling macro
   calls; a macro rec group; a body exception; a nested module; a
   template functor in the body applied downstream; include,
   anonymous and ascribed applications; an alias applied; currying
   with a shared compile side; a body splice calling a body macro. *)

module F (X : sig val v : int end) = struct
  let w = X.v * 2
  macro m () = << X.v + w >>
end
module A = F (struct let v = 10 end)
module B = F (struct let v = 100 end)
let () = Printf.printf "%d %d %d %d\n" $(A.m ()) $(B.m ()) A.w B.w

module Env (X : sig val v : int end) = struct
  exception E of int
  module Sub = struct let s = X.v + 1 end
  macro base () = << X.v >>
  macro m () = << $(base ()) + Sub.s >>
  macro rec pow n = if n = 0 then << 1 >> else << X.v * $(pow (n-1)) >>
  let catch = try raise (E X.v) with E n -> n
end
module EA = Env (struct let v = 3 end)
module EB = Env (struct let v = 5 end)
let () =
  Printf.printf "%d %d %d %d %d %d\n"
    $(EA.m ()) $(EB.m ()) $(EA.pow 2) $(EB.pow 3) EA.catch EB.catch

module T (X : sig val v : int end) = struct
  let base = X.v * 10
  module Tem[Y : sig val u : int end] = struct let r = base + Y.u end
end
module TA = T (struct let v = 1 end)
module TB = T (struct let v = 2 end)
module IA = TA.Tem[struct let u = 5 end]
module IB = TB.Tem[struct let u = 7 end]
let () = Printf.printf "%d %d\n" IA.r IB.r

module N = F (struct let v = 6 end)
include F (struct let v = 2 end)
module _ = F (struct let v = 9 end)
module N2 : sig val w : int end = F (struct let v = 3 end)
module G = F
module N3 = G (struct let v = 11 end)
let () =
  Printf.printf "%d %d %d %d %d %d\n" $(N.m ()) N.w $(m ()) w N2.w $(N3.m ())

module C (X : sig val a : int end) (Y : sig val b : int end) = struct
  let s = X.a + Y.b
  macro cm () = << X.a * 100 + Y.b * 10 + s >>
end
module P = C (struct let a = 1 end)
module C1 = P (struct let b = 2 end)
module C2 = P (struct let b = 3 end)
let () = Printf.printf "%d %d\n" $(C1.cm ()) $(C2.cm ())

module Sp (X : sig val v : int end) = struct
  macro g () = << X.v + 1 >>
  let spliced = $(g ())
  let doubled = spliced * 2
end
module SA = Sp (struct let v = 10 end)
module SB = Sp (struct let v = 20 end)
let () =
  Printf.printf "%d %d %d %d\n" SA.spliced SB.spliced SA.doubled SB.doubled
