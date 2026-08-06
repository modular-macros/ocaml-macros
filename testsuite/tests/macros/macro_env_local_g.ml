(* TEST
 flags = "-g";
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

(* As macro_env_local.ml, under -g: macro bodies at stage -1 must carry no
   debug events, so that closure-converting them over their environments
   sees bare functions. *)

let helper x = x + 1

macro gen () = << helper 5 >>

let z = $(gen ())

macro outer () = << 10 + $(gen ()) >>

let w = $(outer ())

let twice x = x * 2

macro both () = << $(gen ()) + twice 3 >>

let v = $(both ())

let () = Printf.printf "z = %d\nw = %d\nv = %d\n" z w v
