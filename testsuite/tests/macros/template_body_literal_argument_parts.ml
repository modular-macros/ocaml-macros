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

(* In-body applications whose literal arguments carry compile-time
   parts of their own (lifted 2026-07-27, closing BODY-MACRO-LIFT.md
   7.1's phased detail): the chain pass binds the literal's stage -1
   translation in the hoisted form, so its macros bind individually
   for the thunks of its own splices and its application items bind
   their pairs in the chain.  Fragment-side, the literal's internal
   binders were already ordinary term binders -- collected, classified
   and delivered at holes like any body binding. *)

macro mac () = << 2 >>
macro twice e = << $e + $e >>
module G[Y : sig val v : int end] = struct let w = Y.v + 1 end
module K[W : sig val u : int end] = struct let m = W.u * 2 end
module V = struct let u = 4 end

module Outer[T : sig val t : int end] = struct
  (* a splice calling an outside macro *)
  module Z1 = G[struct let v = $(mac ()) + T.t end]
  (* the literal's own macro feeding its own splice *)
  module Z2 = G[struct macro g () = << 3 >> let v = $(g ()) + T.t end]
  (* a template application nested inside the literal *)
  module Z3 = G[struct module N = K[V] let v = N.m + T.t end]
  (* hygiene: the splice quotes the literal's own run-time binder *)
  module Z4 = G[struct let a = T.t let v = $(twice << a >>) end]
  (* the literal's macro captures the enclosing template parameter *)
  module Z5 = G[struct macro g () = << T.t * 2 >> let v = $(g ()) end]
  (* a literal inside a literal, one level further down *)
  module Z6 = G[struct
    module N = K[struct macro h () = << 3 >> let u = $(h ()) + T.t end]
    let v = N.m
  end]
  let r = Z1.w + Z2.w + Z3.w + Z4.w + Z5.w + Z6.w
end

module O1 = Outer[struct let t = 10 end]
module O2 = Outer[struct let t = 100 end]
let () = Printf.printf "%d %d\n" O1.r O2.r

(* Curried: the innermost level's chain owns the literal binding. *)
module C[A : sig val a : int end] [B : sig val b : int end] = struct
  module Z = G[struct let v = $(mac ()) + A.a + B.b end]
  let r = Z.w
end
module P = C[struct let a = 10 end]
module D1 = P[struct let b = 100 end]
module D2 = P[struct let b = 200 end]
let () = Printf.printf "%d %d\n" D1.r D2.r
