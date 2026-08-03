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

(* Template applications INSIDE template functor bodies (Example E of
   BODY-MACRO-LIFT.md 5.5): each outer application instantiates the
   inner functor afresh -- the inner body's splices run once per OUTER
   application, inside it (7.2/7.4) -- while the chain makes the inner
   instance's macros callable from later body splices and body macros.
   Arguments exercised: the parameter (G[X]), a body module (G[Q]), an
   enclosing module (G[W]); plus a unit-parameter application (H[]).
   Cross-module chains live in the standalone repository. *)

let scale = 3

module G[Y : sig val base : int  macro gen : unit -> int expr end] = struct
  let a = Y.base * scale        (* captures enclosing [scale] *)
  macro k () = << a + $(Y.gen ()) >>
  let b = $(k ())               (* body splice: value differs per instance *)
  let r = ref a                 (* fresh state per instantiation *)
end

module W = struct let base = 7  macro gen () = << 1 >> end

module F[X : sig val base : int  macro gen : unit -> int expr end] = struct
  module Q = struct let base = 100  macro gen () = << 9 >> end
  module M0 = G[X]              (* argument = the parameter *)
  module M1 = G[Q]              (* argument = a body module *)
  module MW = G[W]              (* argument = an enclosing module *)
  let c = M0.a + M1.a + MW.a    (* the instances' values in body code *)
  let d = $(M0.k ())            (* an instance's macro in a body splice *)
  macro both () = << $(M0.k ()) + c >>  (* ... and in a body macro *)
  let e = $(both ())
end

module H[] = struct let u = $( << 5 >> ) end

module V1 = struct let base = 10  macro gen () = << 4 >> end
module V2 = struct let base = 20  macro gen () = << 6 >> end

module A = F[V1]
module B = F[V2]
module MH = H[]

(* Fresh state per inner instantiation, per outer application. *)
let () = A.M0.r := !A.M0.r + 1
let () = B.M0.r := !B.M0.r + 1000

(* The doubly instantiated macros, called from a later top-level
   splice through the record chain. *)
let y = $(A.both ()) + $(B.M0.k ())

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n"
    A.M0.a A.M0.b A.M1.b A.MW.b A.c A.d A.e
    B.M0.a B.M0.b B.c B.d B.e
    !A.M0.r !B.M0.r y MH.u
