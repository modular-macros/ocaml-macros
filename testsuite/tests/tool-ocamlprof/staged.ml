(* TEST
 output = "staged.output";
 script = "sh ${test_source_directory}/staged.sh ${ocamlrun} ${ocamlsrcdir} \
           ${test_source_directory}/staged.ml ${output}";
 script;
 check-program-output;
*)

(* ocamlprof instruments run-time code only: not quoted code, a splice
   outside a quotation, a static binding or a generator.  The splices
   in a quotation are run-time code. *)

let f x = if x > 0 then x else - x
let q =
  << fun y -> if y > 0 then $(if f 1 > 0 then Expr.int 1 else Expr.int 2)
              else 0 >>
$let s = if true then 1 else 2
$let[..] g x = if s > 0 then << $x + 1 >> else x
let k = $(if s > 0 then g (Expr.int 3) else Expr.int 4)
let () = print_int (f k); print_newline ()
