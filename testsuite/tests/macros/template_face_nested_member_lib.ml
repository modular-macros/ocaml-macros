(* The library side of template_face_nested_member.ml
   (frex bug 5): an ascribed template functor whose face
   narrows a nested template member (PS) whose OWN result carries
   another surviving template member (Eva).  The ascription coercion
   wraps PS's component; the wrap's dynamic half coerces PS's
   run-time fragment as code, and must do so at stage 0 -- Eva's slot
   in that fragment is its captured-environment tuple and passes
   through untouched.  Coercing it at the call site's compile-time
   stage instead wrapped the tuple as a component function, and any
   consumer completing the chain read closure innards as the tuple at
   module initialisation.

   Eva's body references TWO enclosing binders (IX and JX) so its
   tuple has two roots: the JX application's prologue projects field
   1, which lands on the wrapped closure's closinfo immediate -- the
   byte interpreter's field-read-on-immediate death, at startup. *)

module type ALG = sig
  type t
  macro unit : unit -> t
  macro (<*>) : t -> t -> t
end

module Inner [X : ALG] = struct
  macro var : X.t -> X.t = fun x -> X.(<*>) x (X.unit ())
  module Bind [C : ALG] = struct
    macro (>>=) : C.t -> C.t -> C.t = fun a b -> C.(<*>) a b
  end
end

module type FACE = sig
  module PS [X : ALG] : sig
    macro sta : X.t -> X.t
    module Eva [C : ALG] : sig
      macro eva : C.t -> C.t -> C.t
    end
  end
end

module F [M : ALG] : FACE = struct
  module PS [X : ALG] = struct
    module IX = Inner[X]          (* hidden by the face *)
    module JX = Inner[X]          (* second root for Eva's tuple *)
    macro sta : X.t -> X.t = fun x -> IX.var x
    module Eva [C : ALG] = struct
      module BI = IX.Bind[C]      (* hidden by the face *)
      module BJ = JX.Bind[C]      (* hidden by the face *)
      macro eva : C.t -> C.t -> C.t =
        fun a b -> BJ.(>>=) (BI.(>>=) a b) b
    end
  end
end
