(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* compile-time-objects (LIMITATIONS.md) has two halves.  Objects and
   classes inside quotations, macro bodies and splice bodies are rejected
   by translcore, which quote_objects_rejected.ml pins.  This is the
   other one: a class ITEM in a template functor body, rejected by
   translmod's check_template_body, whose expression walk reuses the same
   check but which sees the item first and says where it is.  A template
   functor body is translated twice, once per world, and the method-cache
   bindings the object translation hoists to the enclosing item have no
   home in the compile-time one. *)

module type S = sig val v : int end

module F [X : S] = struct
  class c = object method m = X.v end
end
