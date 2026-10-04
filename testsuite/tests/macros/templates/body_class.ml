(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A class in a template body is refused: the body is translated
   once per world, and the method-cache bindings that the translation
   of a class hoists to the enclosing item have no place in the
   compile-time one.  Objects in staged code are refused by the
   expression translator (quote_objects_rejected.ml). *)

module type S = sig val v : int end

module F $(X : S) = struct
  class c = object method m = X.v end
end
