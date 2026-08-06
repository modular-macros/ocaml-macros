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

(* Aliasing a mixed functor's parameter inside its body
   (frex bug 2; the trail is in git history): the argument-blind shared-block walk gives a
   macro-free module item a DUMMY slot -- translating it would
   reference the parameter, which has no binding in that world.
   Covers the plain alias (with macros capturing values through it,
   per application), the wrapped variant, and a template-functor
   member whose macro reaches the alias. *)

(* the report's variants, with observable values *)
module type S = sig type t val v : t end
module F (A0 : S) = struct
  module A = A0
  macro id : A.t -> A.t = fun x -> x
  macro getv () = << A.v >>
  let direct = $( getv () )
end
module FI = F (struct type t = int let v = 11 end)
module FJ = F (struct type t = int let v = 22 end)
let () = Printf.printf "%d %d %d\n" FI.direct FJ.direct $( FI.getv () )

(* wrapped variant *)
module G (A0 : S) = struct
  module A = struct module T = A0 end
  macro g () = << A.T.v >>
  let w = $( g () )
end
module GI = G (struct type t = int let v = 7 end)
let () = Printf.printf "%d %d\n" GI.w $( GI.g () )

(* macro inside a template-functor member referencing the alias *)
module H (A0 : S) = struct
  module A = A0
  module T[B : sig val u : int end] = struct
    let r = B.u
    macro tm () = << A.v >>
  end
end
module HI = H (struct type t = int let v = 5 end)
module HT = HI.T[struct let u = 3 end]
let () = Printf.printf "%d %d\n" HT.r $( HT.tm () )
