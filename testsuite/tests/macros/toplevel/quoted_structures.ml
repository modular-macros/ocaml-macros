(* TEST
 toplevel;
*)

(* A quotation reaches a run-time binding through a binding operator,
   through a module that it binds or packs, and a quoted structure
   defines a module. *)

let ( let* ) x f = f x;;
let ( and* ) x y = (x, y);;

$let[..] mlet () = << let* y = 3 in y + 1 >>;;
$let[..] mand () = << let* y = 3 and* z = 4 in y + z >>;;

let a = $(mlet ());;
let b = $(mand ());;

module M = struct let v = 5 end;;
module type S = sig val v : int end;;

$let[..] mmod () = << let module N = M in N.v >>;;
$let[..] mpack () = << (module M : S) >>;;

(* A module bound inside the quotation is not a run-time name. *)
$let[..] mchain () = << let module N = M in let module P = N in P.v >>;;

let c = $(mmod ());;
let d = let module P = (val $(mpack ()) : S) in P.v;;
let e = $(mchain ());;

Printf.printf "a=%d b=%d c=%d d=%d e=%d\n" a b c d e;;
$let[..] built x = << let module M = struct let a = $x + 1 module N = struct let b = a * 2 end end in M.N.b >>;;
let f = $(built << 2 >>);;
