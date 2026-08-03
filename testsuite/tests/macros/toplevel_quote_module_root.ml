(* TEST
 toplevel;
*)

(* The toplevel half of the batch quote_module_root test: a quotation
   reaching a run-time binding through a binding operator, through [let
   module N = M], or through a first-class module pack.  The toplevel
   closure-converts a macro over the same captured roots batch does, so
   the collector's blind spot surfaced here as a misleading "variable
   escaping its quotation's scope" -- pointing at hygiene rather than at
   an uncaptured name -- where batch's static program segfaulted. *)

let ( let* ) x f = f x;;
let ( and* ) x y = (x, y);;

macro mlet () = << let* y = 3 in y + 1 >>;;
macro mand () = << let* y = 3 and* z = 4 in y + z >>;;

let a = $(mlet ());;
let b = $(mand ());;

module M = struct let v = 5 end;;
module type S = sig val v : int end;;

macro mmod () = << let module N = M in N.v >>;;
macro mpack () = << (module M : S) >>;;

(* A module bound inside the quotation is not a run-time name. *)
macro mchain () = << let module N = M in let module P = N in P.v >>;;

let c = $(mmod ());;
let d = let module P = (val $(mpack ()) : S) in P.v;;
let e = $(mchain ());;

Printf.printf "a=%d b=%d c=%d d=%d e=%d\n" a b c d e;;
