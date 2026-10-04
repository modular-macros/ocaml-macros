(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A quotation in a template body's static may not capture the
   parameter's run-time members: the static is code at rest, with no
   application's environment to complete it.  (Typing accepts [X.w]
   inside the quotation, at level 0; the translator's static-quotes
   check refuses the capture.) *)

module type S = sig val w : int end

module F $(X : S) = struct
  $let q = << X.w + 1 >>
end
