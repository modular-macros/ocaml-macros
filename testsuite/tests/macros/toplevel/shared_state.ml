(* TEST
 toplevel;
*)

(* The compile-time part of a phrase runs in the toplevel's process,
   and shares the state of the standard library with run-time code:
   the static bindings below seed the generator of the run-time code. *)

$let () = Random.init 7;;
let x = Random.int 1000000;;

(* exit in compile-time code is a failure of the phrase, as it is in
   batch compilation, and the session goes on. *)

let y = $(exit 3);;
$let s = exit 4;;
let z = 1;;

(* So is an exit that the code catches. *)

$let t = try exit 5 with _ -> 0;;
let u = $(try exit 6 with _ -> Expr.int 0);;
let v = 2;;
