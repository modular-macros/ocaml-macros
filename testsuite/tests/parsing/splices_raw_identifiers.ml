(* TEST
 flags = "-dparsetree -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A raw identifier after a dollar splices the variable that it names,
   a keyword included. *)

let a = << $\#if + $\#match + $\#foo >>
