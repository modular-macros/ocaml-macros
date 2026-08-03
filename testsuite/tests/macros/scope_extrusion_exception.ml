(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Scope extrusion with exceptions: [<<x>>] is carried out of its
   quotation in an exception's payload and spliced, so the built code has
   a free variable; reported as a located error. *)
exception E of int expr

macro m2 () =
  try
    let _ = << fun x -> $(raise (E <<x>>)) >> in << 0 >>
  with E x -> x

(* <<x>> ---> check x bound; (Var x, {x}) *)
let z = $(m2 ())
