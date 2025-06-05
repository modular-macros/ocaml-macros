(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* Scope extrusion with exceptions *)
exception E of int expr

macro m2 () =
  try 
    let _ = << fun x -> $(raise (E <<x>>)) >> in << 0 >>
  with E x -> x

(* <<x>> ---> check x bound; (Var x, {x}) *)
let z = $(m2 ())