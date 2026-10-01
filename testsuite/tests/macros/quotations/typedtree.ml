(* TEST
 flags = "-dtypedtree -dno-unique-ids -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The typed tree of quotations and splices, top-level splices
   included *)

let a = << fun x -> $(<< x >>) >>
let b = $(<< 1 >>)
