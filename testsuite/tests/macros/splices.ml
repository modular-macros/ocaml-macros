(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

let x = $(<<3>>)
let () = Printf.printf "%d\n" x
