(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "constructors.ml";
 flags = "-I lib -I-static lib -static-use counter.cmo";
 ocamlc.byte;
 all_modules = "lib/counter.cmo constructors.cmo";
 compile_only = "false";
 flags = "";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* The run-time constructors of a unit that is available to
   compile-time code are constructors of that code too, and may be
   rebound there. *)

$let a = try raise (Counter.Limit 1) with Counter.Limit n -> n
let b = try raise (Counter.Limit 2) with Counter.Limit n -> n

exception $Bound = Counter.Limit
exception Bound' = Counter.Limit
$let c = try raise (Counter.Limit 3) with Bound n -> n
let d = try raise (Counter.Limit 4) with Bound' n -> n

let () = Printf.printf "%d %d %d %d\n" $(Expr.int a) b $(Expr.int c) d
