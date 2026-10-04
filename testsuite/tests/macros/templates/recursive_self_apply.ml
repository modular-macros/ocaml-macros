(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Its own name being out of scope in a template's body, the only way
   to write a self-application is through a recursive group, where the
   check on unsafe recursive modules catches it, naming the template as
   the unsafe definition.  The accepted half is recursive.ml. *)

module type S = sig val v : int end

module rec M : sig
  module F : $(X : S) -> sig val w : int end
end = struct
  module F $(X : S) = struct
    module Inner = M.F$(X)
    let w = Inner.w
  end
end
