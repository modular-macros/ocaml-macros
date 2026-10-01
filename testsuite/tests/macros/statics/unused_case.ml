(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 run;
 check-program-output;
*)

(* A match case that the pattern-matching compiler finds unused is dead
   by its form, and its splice does not run: only the splices of the
   other cases print, at compile time. *)

let f = function
  | 0 -> $(print_endline "splice of the first case"; Expr.int 1)
  | 0 -> $(print_endline "splice of the unused case"; Expr.int 2)
  | _ -> $(print_endline "splice of the last case"; Expr.int 3)

let () = Printf.printf "%d %d\n" (f 0) (f 5)
