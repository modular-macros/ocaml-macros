(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An abstract module type may only be matched by a purely level 0
   module type.  Everything that consumes the range calculus treats an
   abstract module type as macro-free, so admitting a macro-bearing one
   makes those answers wrong wherever the abstraction is instantiated:
   inside a functor whose parameter declares the module type, a
   template application's component call would pass unit for an
   argument that really has compile-time components -- a segfault at
   compile-time evaluation (frex bug 4; the trail is in git history).  The
   [with module type] route is guarded by Macro_in_abstract_modtype;
   this is the signature matching route. *)

module type TYPE = sig type t end

module Make (Ops : functor (X : TYPE) -> sig module type OP end) = struct
  module type Algebra = sig module T : TYPE module Op : Ops(T).OP end
end

module Monoid_ops (X : TYPE) = struct
  module type OP = sig macro one : unit -> X.t end
end

module S = Make (Monoid_ops)
