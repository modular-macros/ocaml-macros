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

(* Splices in mixed functor bodies, hardened (SPLITTING-FUNCTORS.md
   stage 5): interleaved splices and macros whose values chain per
   application; a splice inside a nested structure of the body; a
   body splice beside an in-body template application; a mixed
   APPLICATION inside a mixed body with its macro spliced there; a
   binder-quoting macro rec under the body's lambda; and body
   splices reading unit-level spliced values. *)

module type A = sig val v : int end

(* interleaved splices and macros; later splices via env see earlier
   SPLICED values per application *)
module F (X : A) = struct
  macro m1 () = << X.v + 1 >>
  let a = $(m1 ())
  macro m2 () = << a * 2 >>
  let b = $(m2 ())
  let c = a + b
end
module FA = F (struct let v = 10 end)
module FB = F (struct let v = 20 end)
let () = Printf.printf "%d %d %d %d %d %d\n" FA.a FA.b FA.c FB.a FB.b FB.c

(* a splice inside a nested structure of the mixed body *)
module G (X : A) = struct
  macro g () = << X.v * 3 >>
  module Sub = struct
    let sx = $(g ()) + 1
  end
  let d = Sub.sx
end
module GA = G (struct let v = 5 end)
let () = Printf.printf "%d\n" GA.d

(* body splice + in-body template application (level 0 result) *)
module T[Y : A] = struct let tv = Y.v * 10 end
module V = struct let v = 4 end
module H (X : A) = struct
  macro h () = << X.v >>
  module Z = T[V]
  let e = $(h ()) + Z.tv
end
module HA = H (struct let v = 7 end)
let () = Printf.printf "%d %d\n" HA.e HA.Z.tv

(* a mixed APPLICATION inside a mixed body, its macro spliced there *)
module Base (X : A) = struct
  let bw = X.v + 100
  macro bm () = << bw >>
end
module K (X : A) = struct
  module N = Base (struct let v = 1 end)
  let f = $(N.bm ()) + X.v
end
module KA = K (struct let v = 2 end)
module KB = K (struct let v = 3 end)
let () = Printf.printf "%d %d\n" KA.f KB.f

(* binder-quoting splice under the body's lambda *)
module P (X : A) = struct
  macro rec pow n = if n = 0 then << 1 >> else << X.v * $(pow (n-1)) >>
  let g y = y + $(pow 2)
  let r = g 5
end
module PA = P (struct let v = 3 end)
module PB = P (struct let v = 4 end)
let () = Printf.printf "%d %d\n" PA.r PB.r

(* two mixed functors with splices + a unit-level splice mixing in *)
macro unit_m () = << 1000 >>
let unit_v = $(unit_m ())
module Q (X : A) = struct
  macro q () = << X.v + unit_v >>
  let s = $(q ())
end
module QA = Q (struct let v = 5 end)
let () = Printf.printf "%d %d\n" unit_v QA.s
