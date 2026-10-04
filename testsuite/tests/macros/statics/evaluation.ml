(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 run;
 check-program-output;
*)

(* The static bindings of a unit are evaluated when the unit is
   compiled, once and in order, before its splices; what they print is
   printed by the compiler. *)

$let x = print_endline "x"; 1
let a = $(print_endline "splice of a"; Expr.int x)
$let y = print_endline "y"; x + 1
let b = $(print_endline "splice of b"; Expr.int (x + y))

let () = Printf.printf "%d %d\n" a b
