(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A macro brought in by including a literal structure is bound at the
   compile-time stage, so a splice may use it.  Previously the static pass
   dropped includes and the macro was never bound. *)

include struct
  let base = 100
  macro m () = << base + 7 >>
end

let v = $(m ())

(* A macro defined normally still works alongside the included one. *)
macro n () = << 3 >>
let w = $(n ())

let () = Printf.printf "v = %d\nw = %d\n" v w
