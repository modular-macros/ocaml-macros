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

(* In-body template applications with MIXED results
   (SPLITTING-FUNCTORS.md, S1 lifted for top-level plain bodies):
   the application's record binds at its slot in the enclosing
   shared block, so the instantiation's macros project and splice
   through the functor's applications -- including the include form,
   and combined with a macro-bearing BODY-LOCAL argument whose
   per-application environment flows through the instantiation's
   macros.  The nested residual (a plain body under a template body)
   stays guarded. *)

(* S1: in-body template application with a MIXED result *)
module T[Y : sig val u : int end] = struct
  let tv = Y.u * 10
  macro tm () = << Y.u + tv >>
end
module V = struct let u = 4 end
module H (P : sig val p : int end) = struct
  module Z = T[V]
  let h = Z.tv + P.p
end
module HA = H (struct let p = 1 end)
module HB = H (struct let p = 2 end)
let () = Printf.printf "%d %d %d %d\n" HA.h HB.h $(HA.Z.tm ()) HA.Z.tv

(* include form; body-local macro-bearing argument; per-application
   env values through the instantiation's macros *)
module T2[Y : sig val u : int macro g : unit -> int expr end] = struct
  let tv = Y.u * 10
  macro tm () = << Y.u + tv + $( Y.g () ) >>
end
module H2 (P : sig val p : int end) = struct
  module W2 = struct let u = P.p macro g () = << u * 100 >> end
  module Z2 = T2[W2]
  include T2[W2]
  let h = Z2.tv + tv
end
module H2A = H2 (struct let p = 2 end)
module H2B = H2 (struct let p = 3 end)
let () =
  Printf.printf "%d %d %d %d\n" H2A.h H2B.h $(H2A.Z2.tm ()) $(H2B.tm ())
