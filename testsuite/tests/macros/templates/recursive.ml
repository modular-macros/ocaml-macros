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

(* A template may not apply itself inside its own body -- its own name
   is not in scope there, as for any [module] binding -- and a
   recursive group that does so is refused by the ordinary check on
   unsafe recursive modules (recursive_self_apply.ml).  A recursive
   group may bind a template; applied from outside the group, through
   the group's name, the definition and the application go through the
   ordinary paths. *)

module type S = sig val v : int end

module rec M : sig
  module F : $(X : S) -> sig val w : int end
  val k : int
end = struct
  module F $(X : S) = struct let w = X.v + 1 end
  let k = 7
end

module V = struct let v = 41 end

module A = M.F$(V)

let () = Printf.printf "%d %d\n" A.w M.k
