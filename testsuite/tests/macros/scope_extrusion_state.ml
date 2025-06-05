(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* Extrusion with references *)
macro m () =
  let r = ref << 0 >> in
  let _ = << fun x -> $(r := <<x>> ; <<x>>) >> in
  !r

let y = $(m ()) 