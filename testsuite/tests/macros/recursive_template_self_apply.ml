(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The rejected half of recursive-templates (LIMITATIONS.md).  Its own
   name being out of scope in a template functor's body, the only way to
   write a self-application is through a recursive group -- and there the
   standard unsafe-recursive-module check already catches it, naming the
   functor as the unsafe definition.  No staging-specific diagnostic is
   needed, which is why none exists; this test is what says so.  The
   accepted half, a group that binds a template functor without applying
   it to itself, is recursive_templates.ml. *)

module type S = sig val v : int end

module rec M : sig
  module F : [X : S] sig val w : int end
end = struct
  module F [X : S] = struct
    module Inner = M.F[X]
    let w = Inner.w
  end
end
