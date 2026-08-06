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

(* Template functor APPLICATION (3B-SCHEME step 4), same unit: the
   scheme's worked example, within the v1 restrictions.  The body's
   splices run once per application (per-instantiation values and
   state); a later splice calls the instantiated macro; body splices
   call the argument's macros and body macros.  Cross-module
   application and dependents are exercised in the standalone
   repository (tests/bytecode/template_apply). *)

let scale = 3
module F[X : sig val base : int macro gen : unit -> int expr end] = struct
  let v = X.base * scale
  macro m () = << v + 1 >>
  let w = $(m ()) + $(X.gen ())
  let r = ref v
end

module V = struct
  let base = 10
  macro gen () = << 4 >>
end
module V2 = struct
  let base = 100
  macro gen () = << 7 >>
end

module M = F[V]
module M2 = F[V2]

(* Fresh state per instantiation. *)
let () = M.r := !M.r + 1

(* The instantiated macros, called like anyone else's. *)
let y = $(M.m ()) + $(M2.m ())

(* A unit parameter applies as G[]. *)
module G[] = struct let u = $( << 5 >> ) end
module MG = G[]

let () =
  Printf.printf "%d %d %d %d %d %d %d %d\n"
    M.v M.w M2.v M2.w !M.r !M2.r y MG.u
