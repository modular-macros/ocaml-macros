(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Mixed functor TYPES in a compiled unit: signatures mentioning them
   compile, including nested and curried forms and the
   functor-to-template shape (stage 1 of SPLITTING-FUNCTORS.md). *)

module type A = sig val v : int end
module type Mixed = (X : A) -> sig val w : int macro m : unit -> int expr end
module type Curried = (X : A) -> (Y : A) -> sig macro m : unit -> int expr end
module type ToTemplate = (X : A) -> [Y : A] sig val r : int end
module type Nested = sig
  module F : Mixed
  module G : ToTemplate
end

let () = print_string "ok\n"
