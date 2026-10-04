(* TEST
 toplevel;
*)

(* A phrase that fails leaves nothing behind: neither what its
   compile-time part bound, nor what its run-time part did. *)

$let x = "before";;
let y = "before";;

(* The compile-time part fails after rebinding x. *)
$let x = 1  let a : int = $(failwith "at compile time");;
let b = $(Expr.string x);;

(* The run-time part fails after the compile-time part rebound x, and
   after it rebound y itself. *)
$let x = 2  let y = 3  let c : int = failwith "at run time";;
let d = $(Expr.string x) ^ y;;

(* A generator that raises *)
$let[..] g () : int expr = raise Not_found;;
let e = $(g ());;
let f = 4;;
