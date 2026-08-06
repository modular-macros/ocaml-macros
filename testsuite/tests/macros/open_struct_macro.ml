(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* [open struct ... end] beside a macro.  The items an open binds occupy
   run-time positions, so at the compile-time stage they must get dummy
   slots -- previously they were dropped and the macro block's coercion
   indexed past the end of its fields. *)

open struct let x = 1 end

macro m () = << 2 >>

let y = x

let v = y + $(m ())

(* An open after the macro, and bindings on both sides of a splice. *)

open struct
  let a = 10
  let b = 20
end

let w = a + b + $(m ())

let () = Printf.printf "v = %d\nw = %d\n" v w
