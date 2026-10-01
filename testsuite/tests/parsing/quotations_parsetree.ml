(* TEST
 flags = "-dparsetree -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The parse tree of quotations and splices *)

let a = << f $x $M.y $(g << z >>) >>
