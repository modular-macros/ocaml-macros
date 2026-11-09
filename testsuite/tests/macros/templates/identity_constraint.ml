(* TEST
 readonly_files = "identity_constraint_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "identity_constraint_lib.ml identity_constraint.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "identity_constraint_lib.ml identity_constraint.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A template applied to a sibling module, in a module whose constraint
   leaves its block as it is.  The native compiler stores the components
   of the module in the block of the unit, as it does without the
   constraint, and the application's environment reads its argument
   there. *)

module M : sig
  module D : sig val x : int end
  module G : Identity_constraint_lib.S
end = struct
  module D = struct let x = 1 end
  module G = Identity_constraint_lib.T$(D)
end

let () = Printf.printf "%d %d\n" M.D.x M.G.x
