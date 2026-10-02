(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml lib/both.ml";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "both_worlds.ml";
 flags = "-I lib -I-static lib -static-use counter.cmo -static-use both.cmo";
 ocamlc.byte;
 all_modules = "lib/counter.cmo lib/both.cmo both_worlds.cmo";
 compile_only = "false";
 flags = "";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* The units of a directory given to -I and to -I-static are available
   to compile-time code and to run-time code.  Compile-time code may
   use their run-time components and their compile-time components. *)

$let a = Both.v + Both.s + Both.Sub.w + Both.Sub.t
$let[..] b () = Both.add (Expr.int Both.v)

open Both
$let c = v + s + Sub.w + Sub.t
$let[..] d () = let open Sub in add (Expr.int (w + t))
$let[..] quoted () = let open Both in << v + Sub.w >>

module Alias = Both.Sub
module Included = struct include Both.Sub end
$let e = Alias.t + Included.t

let () =
  Printf.printf "%d %d %d %d %d %d %d\n"
    $(Expr.int a) $(b ()) $(Expr.int c) $(d ()) $(quoted ()) $(Expr.int e)
    (v + Alias.w + Included.w)

(* The state of a unit at compile time is not its state at run time. *)
let () =
  Printf.printf "%d %d\n" $(Expr.int (Counter.next ())) (Counter.next ())
