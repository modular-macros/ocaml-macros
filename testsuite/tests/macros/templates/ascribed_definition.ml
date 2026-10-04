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

(* A definition under an ascription, [module F : T = functor $(X : S)
   -> ...]: the coercion applies to the template's value in each
   world. *)

module type S = sig val v : int end
module K = struct let k = 10 end
module type T =
  $(X : S) -> sig
    val w : int
    module M : sig val k : int end
    $val[..] g : unit -> int expr
  end
module F : T = functor $(X : S) -> struct
  let w = X.v + 1
  let hidden = 5
  module M = K
  $let[..] g () = << w + hidden >>
end
module A = F$(struct let v = 1 end)
module F2 = (functor $(X : S) -> struct let w = X.v + 2 end : $(X : S) -> sig val w : int end)
module B = F2$(struct let v = 1 end)
let () = Printf.printf "%d %d %d %d\n" A.w A.M.k $(A.g ()) B.w
