(* TEST
 readonly_files = "dummy_alias_projection.mli";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "dummy_alias_projection.mli dummy_alias_projection.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "dummy_alias_projection.mli dummy_alias_projection.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A module alias whose path roots at a member the compile-time stage
   dummies.  [App] is a plain functor application -- macro-free, so its
   compile-time slot is a dummy (see [Translmod.dummy_slots]) -- and the
   .mli lists [Infix], so the block-build coercion's [Tcoerce_alias] arm
   translates the path [App.Sub].  That translation compiled to a field
   read of an immediate, and everything that ran the block's code
   crashed: the unit's own static program here (the splice forces one),
   a consumer's static program when its splice called [m], and the
   toplevel when #load probed and ran the unit's macros object.  A path
   rooted at a dummy now translates to the dummy itself. *)

module F (X : sig val x : int end) = struct
  module Sub = struct let y = X.x end
end

module App = F (struct let x = 1 end)

module Infix = App.Sub

macro m : int expr -> int expr = fun x -> x

let w = $( << 3 >> )

let () = Printf.printf "%d %d\n" Infix.y w
