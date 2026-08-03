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

(* Literal template arguments with compile-time parts of their own
   (lifted 2026-07-27): in batch contexts a literal argument may
   contain top-level splices and template applications -- the
   three-pass pattern recurses one level down through the hoisted
   stage -1 binding, which puts the literal's macros and its inner
   applications' registry bindings in scope at the emit call
   (BODY-MACRO-LIFT.md 7.1's phased detail).  In-body applications
   (inside template functor bodies) keep the restriction. *)

macro mac () = << 7 >>
module F[X : sig val x : int end] = struct let out = X.x + 1 end
module G[Y : sig val v : int end] = struct let w = Y.v * 2 end
module V = struct let v = 3 end

(* A splice calling an outside macro. *)
module A1 = F[struct let x = $(mac ()) end]

(* The literal's own macro feeding its own splice. *)
module A2 = F[struct macro g () = << 5 >> let x = $(g ()) end]

(* Hygiene: the splice quotes the literal's own run-time binder. *)
macro twice e = << $e + $e >>
module A3 = F[struct let a = 5 let x = $(twice << a >>) end]

(* A template application nested inside a macro-free literal: the
   stage -1 walk runs for the inner registry bindings even though the
   record call passes unit. *)
module A4 = F[struct module Z = G[V] let x = Z.w end]

(* Mixed compile-time parts: an own macro and a nested application. *)
module A5 = F[struct
  macro h () = << 4 >>
  module Z = G[V]
  let x = Z.w + $(h ())
end]

(* A coerced literal with a splice. *)
module A6 = F[(struct let x = $(mac ()) let extra = 99 end :
                 sig val x : int end)]

(* A literal inside a literal: the recursion one level further. *)
module A7 = F[struct
  module Z = G[struct macro g () = << 2 >> let v = $(g ()) + 1 end]
  let x = Z.w
end]

(* Include with a splice-bearing literal argument. *)
include F[struct let x = $(mac ()) * 10 end]

let () =
  Printf.printf "%d %d %d %d %d %d %d %d\n"
    A1.out A2.out A3.out A4.out A5.out A6.out A7.out out

(* The same shapes under a PLAIN functor body: the literals bind at
   the hoisted call, their fragments reach the parameter
   per-application through the environment pair. *)
module H (P : sig val p : int end) = struct
  module Z1 = G[struct let v = $(mac ()) + P.p end]
  module Z2 = G[struct macro g () = << 5 >> let v = $(g ()) + P.p end]
  module Z3 = G[struct module N = G[V] let v = N.w + P.p end]
  let s = Z1.w + Z2.w + Z3.w
end

module B1 = H (struct let p = 10 end)
module B2 = H (struct let p = 100 end)

let () = Printf.printf "%d %d\n" B1.s B2.s
