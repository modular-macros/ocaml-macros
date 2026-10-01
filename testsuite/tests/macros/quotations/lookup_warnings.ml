(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The warnings of opens, under lookup by level: an open whose names are
   used only at a level that it does not serve is unused, and an open
   in compile-time code shadows a name that serves that code. *)

module M = struct let print_endline (_ : int) = () end
open M
let a = $(print_endline "a"; << 1 >>)

module N = struct let length = 0 end
let b = $(let open N in Expr.int (List.length [1]))

let c = let open List in length [1] + $(let open List in Expr.int (length [1]))
