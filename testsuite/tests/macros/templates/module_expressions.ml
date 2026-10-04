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

(* A template application inside the module expression of an item --
   in a plain application, under a constraint, as the body of a functor
   -- is bound as a hidden application item around the item. *)

module F$(X : sig val a : int end) = struct
  let s = X.a + 1
  let () = Printf.printf "[F %d]" X.a
  $let[..] g () = << s >>
end
module G (Y : sig val s : int end) = struct let t = Y.s * 2 end

(* In a plain application; under a constraint; in an include and an
   open. *)
module M1 = G (F$(struct let a = 2 end))
module V = struct let a = 5 end
module M2 = (G (F$(V)) : sig val t : int end)
module M3 = G ((F$(struct let a = 3 end) : sig val s : int end))
include G (F$(struct let a = 1 end))
open G (F$(struct let a = 10 end))
let () = Printf.printf "%d %d %d %d\n" M1.t M2.t M3.t t

(* An inline chain of applications of both kinds, and a template
   application as the functor of a plain application. *)
module C (A : sig val a : int end) $(B : sig val b : int end)
    (D : sig val d : int end) $(E : sig val e : int end) =
struct let v = A.a * 1000 + B.b * 100 + D.d * 10 + E.e end
module P = C (struct let a = 5 end)$(struct let b = 6 end)
             (struct let d = 7 end)$(struct let e = 8 end)
module H$(X : sig val a : int end) (Y : sig val b : int end) = struct
  let v = X.a * 10 + Y.b
end
module Q = H$(struct let a = 1 end) (struct let b = 2 end)
let () = Printf.printf "%d %d\n" P.v Q.v

(* In a functor body, applied twice; in a template body; in a functor
   body in a template body. *)
module K (P : sig val a : int end) = struct module M = G (F$(P)) end
module K1 = K (struct let a = 1 end)
module K2 = K (struct let a = 2 end)
module T$(Z : sig val a : int end) = struct
  module M = G (F$(Z))
  module L (R : sig end) = struct
    module M = G (F$(struct let a = Z.a * 10 end))
  end
  module L1 = L (struct end)
  module P = C (struct let a = Z.a end)$(struct let b = 2 end)
               (struct let d = 3 end)$(struct let e = 4 end)
end
module T1 = T$(struct let a = 3 end)
let () = Printf.printf "%d %d %d %d %d\n" K1.M.t K2.M.t T1.M.t T1.L1.M.t T1.P.v

(* As the body of a functor, sealed or not, with the parameter in the
   argument or not; applied in a template body. *)
module K3 (Y : sig val a : int end) = F$(Y)
module K4 (Y : sig end) = F$(V)
module K5 (Y : sig val a : int end) : sig val s : int end =
  F$(struct let a = Y.a * 2 end)
module K31 = K3 (struct let a = 1 end)
module K32 = K3 (struct let a = 2 end)
module K41 = K4 (struct end)
module K51 = K5 (struct let a = 3 end)
let () = Printf.printf "%d %d %d %d %d\n" K31.s K32.s K41.s K51.s $(K31.g ())

(* A literal functor whose body applies a template, as the argument
   of a functor and of a template. *)
module App (Fn : functor (Y : sig val a : int end) -> sig val s : int end) =
struct
  module R1 = Fn (struct let a = 1 end)
  module R2 = Fn (struct let a = 2 end)
end
module M4 = App (functor (Y : sig val a : int end) ->
                   struct module A = F$(Y) let s = A.s + $(A.g ()) end)
module W$(Fn : functor (Y : sig val a : int end) -> sig val s : int end) =
struct module R = Fn (struct let a = 3 end) end
module M5 = W$(functor (Y : sig val a : int end) ->
                 struct module A = F$(Y) let s = A.s end)
let () = Printf.printf "%d %d %d\n" M4.R1.s M4.R2.s M5.R.s

(* In the body of a recursive module, applied to a sibling. *)
module rec RA : sig val t : int end = struct
  module M = G (F$(RB))
  let t = M.t
end
and RB : sig val a : int end = struct let a = 4 end
let () = Printf.printf "%d\n" RA.t
