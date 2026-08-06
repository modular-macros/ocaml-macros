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

(* D3 lifted: a template functor argument may be a literal structure.
   It is bound to a hidden name inside the application item, in each of
   the coordinated passes, and treated as a path from there on
   (BODY-MACRO-LIFT.md 7.1). *)

(* (a) Scoping soundness: the code m returns must not reference the
   anonymous argument by name from y's position -- X is a captured
   root of m, so the returned code projects X.base through M's slots;
   the only by-name reference to the hidden binder in the emitted term
   is the environment pair at M's own item, inside the in-item
   binding's scope. *)
module F[X: sig val base : int end] = struct macro m () = << X.base >> end
module M = F[struct let base = 3 end]
let y = $(M.m ())

(* (b) A literal argument carrying values AND macros, used by body
   values, body splices, and a later splice calling the instantiated
   macro; (c) two applications with different literal arguments keep
   their instantiations apart. *)
let scale = 3
module G[X : sig val base : int macro gen : unit -> int expr end] = struct
  let v = X.base * scale
  macro mg () = << v + 1 >>
  let w = $(mg ()) + $(X.gen ())
end

module N = G[struct let base = 10 macro gen () = << 4 >> end]
module N2 = G[struct let base = 100 macro gen () = << 7 >> end]

let z = $(N.mg ()) + $(N2.mg ())

(* (d) The functor position lifts by the same move: an immediate
   template functor, both positions literal at once; and a
   unit-parameter literal functor. *)
module P =
  (functor [X : sig val base : int macro gen : unit -> int expr end] ->
     struct
       let v = X.base + 1
       macro mp () = << v * 2 >>
       let w = $(mp ()) + $(X.gen ())
     end)[struct let base = 5 macro gen () = << 100 >> end]
let t = $(P.mp ())

module U = (functor [] -> struct let u = $( << 9 >> ) end)[]

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d %d\n"
    y N.v N.w N2.v N2.w z P.v P.w t U.u
