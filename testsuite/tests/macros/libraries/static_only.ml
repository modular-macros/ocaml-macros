(* TEST
 subdirectories = "lib";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "lib/counter.ml";
   compile_only = "true";
   ocamlc.byte;
   all_modules = "static_only.ml";
   compile_only = "false";
   flags = "-I-static lib -static-use counter.cmo";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "lib/counter.ml";
   compile_only = "true";
   ocamlc.byte;
   all_modules = "static_only.ml";
   compile_only = "false";
   flags = "-I-static lib -static-use counter.cmo";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The units of a directory given to -I-static are available to
   compile-time code, which is linked with the files given to
   -static-use.  The program itself is not linked with them: run-time
   code may use their types, and no more. *)

$let first = Counter.next ()
$let[..] label (v : Counter.t) = Expr.string (Counter.name v)
$let opened = let open Counter in name B

open Counter
$let second = next ()
let both : t list = [A; Counter.B]

let () =
  Printf.printf "%d %s %s %d %d %d\n"
    $(Expr.int first) $(label Counter.A) $(Expr.string opened)
    $(Expr.int second) $(Expr.int (next ())) (List.length both)
