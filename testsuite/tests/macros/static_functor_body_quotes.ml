(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The same for a plain functor's body: the parameter is a run-time
   name the static's quotations may not capture. *)

module F (X : sig val v : int end) = struct
  $let q = << X.v + 1 >>
end
