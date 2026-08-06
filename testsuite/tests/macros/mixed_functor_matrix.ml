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

(* The mixed-functor interaction matrix (SPLITTING-FUNCTORS.md stage
   4): result ascriptions hiding helpers quoted by macros; functor-
   and application-level ascriptions reordering and hiding macros;
   currying at depth 3 through partial chains; a mixed functor
   nested in a mixed body, applied via projection; a mixed
   application result as a TEMPLATE functor argument; a literal
   argument whose macros the parameter signature coerces away; an
   alias of an application result; a generative mixed functor; and a
   mixed functor inside a plain structure. *)

module type A = sig val v : int end

(* 1. result ascription ON the definition, hiding a helper *)
module F (X : A) : sig val w : int macro m : unit -> int expr end = struct
  let secret = X.v + 1
  let w = X.v * 2
  macro m () = << secret * 10 + w >>
end
module N1 = F (struct let v = 3 end)
let () = Printf.printf "%d %d\n" $(N1.m ()) N1.w

(* 2. functor-level ascription reordering/hiding on an alias *)
module type Wide = sig
  val w : int
  macro m : unit -> int expr
  macro extra : unit -> int expr
end
module G (X : A) : Wide = struct
  let w = X.v
  macro m () = << w + 1 >>
  macro extra () = << w + 100 >>
end
module G2 : (X : A) -> sig macro m : unit -> int expr val w : int end = G
module N2 = G2 (struct let v = 7 end)
let () = Printf.printf "%d %d\n" $(N2.m ()) N2.w

(* 3. ascription on the application result, reordering *)
module N3 : sig macro m : unit -> int expr val w : int end =
  G (struct let v = 20 end)
let () = Printf.printf "%d %d\n" $(N3.m ()) N3.w

(* 4. currying depth 3, partial chains *)
module C3 (X : A) (Y : A) (Z : A) = struct
  let s = X.v + Y.v + Z.v
  macro cm () = << X.v * 100 + Y.v * 10 + Z.v + s >>
end
module P1 = C3 (struct let v = 1 end)
module P2 = P1 (struct let v = 2 end)
module NC = P2 (struct let v = 3 end)
let () = Printf.printf "%d %d\n" $(NC.cm ()) NC.s

(* 5. mixed functor nested in a mixed functor's body *)
module Outer (X : A) = struct
  let base = X.v
  macro om () = << base >>
  module Inner (Y : A) = struct
    let t = X.v * Y.v
    macro im () = << X.v * 1000 + Y.v * 100 + t >>
  end
end
module O = Outer (struct let v = 2 end)
module I = O.Inner (struct let v = 3 end)
let () = Printf.printf "%d %d %d\n" $(O.om ()) $(I.im ()) I.t

(* 6. a mixed application result as a TEMPLATE argument *)
module T[M : sig val w : int macro m : unit -> int expr end] = struct
  let r = $( M.m () ) + M.w
end
module TN = T[N1]
let () = Printf.printf "%d\n" TN.r

(* 7. literal argument with coerced-away macros *)
module H (X : A) = struct
  let hw = X.v + 1
  macro hm () = << hw >>
end
module NL = H (struct macro dead () = << 0 >> let v = 41 end)
let () = Printf.printf "%d %d\n" $(NL.hm ()) NL.hw

(* 8. alias of an application result; generative mixed functor *)
module AL = N1
module Gen () = struct
  let gv = 5
  macro gm () = << gv * 3 >>
end
module NG = Gen ()
let () = Printf.printf "%d %d\n" $(AL.m ()) $(NG.gm ())

(* 9. a mixed functor inside a plain structure, applied via projection *)
module Box = struct
  module BF (X : A) = struct
    let bw = X.v - 1
    macro bm () = << bw * 2 >>
  end
end
module NB = Box.BF (struct let v = 6 end)
let () = Printf.printf "%d %d\n" $(NB.bm ()) NB.bw
