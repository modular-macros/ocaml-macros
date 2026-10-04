(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An attribute does not make a value a static binding: [s] is a
   run-time value, which compile-time code cannot use. *)

let (s [@ocaml.static]) = "hello"
let y = $(Expr.string (s ^ " world"))
