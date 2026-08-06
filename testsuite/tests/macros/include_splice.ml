(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* A macro defined and spliced inside the same [include struct .. end]:
   the include's bindings are hoisted over the rest of the compile-time
   part, so the splice's code -- evaluated at the static program's emit
   point -- has the macro in scope.  (Formerly the D-auto-15 limitation:
   the splice was collected outside the include's scope.) *)

include struct
  macro m () = << 21 >>
  let v = $(m ()) * 2
end

(* And through a nested module inside the include. *)
include struct
  module N = struct
    macro g () = << 5 >>
  end
  let w = $(N.g ()) + v
end

let () = Printf.printf "v = %d\nw = %d\n" v w
