(* TEST
 flags = "-no-alias-deps";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 program = "open_aliases.cmo";
 ocamlobjinfo;
 check-program-output;
*)

(* Aliases of units that the open of the standard library brings into
   scope: their interfaces are not read, and the units are not
   imported. *)

module M = List
module H = Hashtbl
let x = 1
