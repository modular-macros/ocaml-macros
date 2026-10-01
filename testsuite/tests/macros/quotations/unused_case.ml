(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 run;
 check-program-output;
*)

(* A match case that the pattern-matching compiler finds unused is dead
   by its form, in a quotation as at the top level: when the quotation
   is evaluated, the splices of the other cases run, and the unused
   case's does not.  The order of separate splices is unspecified, so
   the names are sorted. *)

let ran = ref []

let note name =
  ran := name :: !ran;
  Expr.int 0

let code (n : int expr) =
  << match $n with
     | 0 -> $(note "first case")
     | 0 -> $(note "unused case")
     | _ -> $(note "last case") >>

let () =
  ignore (code << 1 >>);
  List.iter print_endline (List.sort compare !ran)
