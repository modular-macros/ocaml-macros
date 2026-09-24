(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* https://github.com/modular-macros/macros-bugs/blob/master/bugs/splice-indexing/test.ml *)
let a = $(<<1>>)
let b = $(<<"2">>)
let c = $(<<'3'>>)
let () = assert (Printf.sprintf "%d %S %C" a b c = "1 \"2\" '3'")
