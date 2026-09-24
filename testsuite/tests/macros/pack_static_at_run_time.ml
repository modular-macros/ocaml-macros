(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The other direction of relative pack matching (PACK-MATCHING.md
   §2's table): a static module packed AT RUN TIME.  The static's slot
   in the run block is a placeholder, and the recorded-level rule at
   ambient 0 refuses it as a world mismatch. *)

module type S = sig val v : int end
module K = struct $let v = 41 end
let p = (module K : S)
