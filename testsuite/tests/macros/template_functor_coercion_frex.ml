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

(* frex bug 3's forms 2 and 3 verbatim (trail in git history) (form 1, the cross-unit
   .mli, lives in the tests repository): an inline ascription over a
   nested template member applying a template over the captured
   parameter, and the same narrowing through a template PARAMETER's
   signature -- both crashed the static program before the face-tagged
   coercions coerced the component pair. *)

module type S = sig type t macro id : t -> t end
module K [P : S] = struct
  macro pick : P.t -> P.t = fun x -> P.id x
end
module F [A : S] : sig
  module G [B : S] : sig macro use : B.t -> B.t end
end = struct
  module G [B : S] = struct
    module K0 = K[A]
    macro use : B.t -> B.t = fun x -> B.id x
  end
end
module M = struct type t = int macro id : t -> t = fun x -> x end
module FA = F[M]
module GA = FA.G[M]
let () = Printf.printf "%d\n" $( Expr.int (GA.use 5) )

module type S3 = sig type t macro id : t -> t end
module W3 [P3 : S3] = struct macro w : P3.t -> P3.t = fun x -> P3.id x end
module type FX3 = sig
  module G3 [B3 : S3] : sig macro use : B3.t -> B3.t end
end
module F3 [A3 : S3] = struct
  module G3 [B3 : S3] = struct
    module W03 = W3[A3]
    macro use : B3.t -> B3.t = fun x -> B3.id x
  end
end
module H3 [E3 : FX3] [C3 : S3] = struct
  module GA3 = E3.G3[C3]
  macro h : C3.t -> C3.t = fun x -> GA3.use x
end
module M3 = struct type t = int macro id : t -> t = fun x -> x end
module FM3 = F3[M3]
module H03 = H3[FM3]
module HM3 = H03[M3]
let () = Printf.printf "%d\n" $( Expr.int (HM3.h 5) )
