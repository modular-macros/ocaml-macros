(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* https://github.com/modular-macros/macros-bugs/blob/master/bugs/function-argument-order/bug.ml *)
let s = Printf.sprintf "%d %d %d" $(<<1>>) $(<<2>>) $(<<3>>)
let () = assert (s = "1 2 3")
