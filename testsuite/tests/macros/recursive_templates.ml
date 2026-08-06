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

(* A template functor may not apply itself inside its own body -- its own
   name is not in scope there, as for any [module] binding -- and
   LIMITATIONS.md's recursive-templates records that no dedicated
   diagnostic is needed for that, because [module rec] already behaves.
   This is the accepted half: a recursive group may BIND a template
   functor, and as long as no binding applies it to itself the group
   compiles, links and runs like any other.  The functor is applied from
   outside the group, through the group's own name, so the definition and
   the application go through the ordinary paths.  The half that must
   fail is recursive_template_self_apply.ml. *)

module type S = sig val v : int end

module rec M : sig
  module F : [X : S] sig val w : int end
  val k : int
end = struct
  module F [X : S] = struct let w = X.v + 1 end
  let k = 7
end

module V = struct let v = 41 end

module A = M.F[V]

let () = Printf.printf "%d %d\n" A.w M.k
