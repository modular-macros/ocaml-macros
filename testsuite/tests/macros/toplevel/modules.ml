(* TEST
 toplevel;
*)

(* Modules in compile-time code in the toplevel *)

module type S = sig val v : int end;;
$let packed = (module struct let v = 1 end : S);;
$let unpacked = let module M = (val packed) in M.v;;
$let nested = let module M = struct module N = struct let w = 2 end end in M.N.w;;
module Static = struct $let v = 3  let r = 4 end;;
$let aliased = let module A = Static in A.v;;
let at_run_time = let module A = Static in A.r;;
let spliced = $(let module M = (val packed) in Expr.int M.v);;
$let[..] shown (module M : S) = Expr.int M.v;;
$(shown (module struct let v = 5 end));;
module R = struct let v = 6 end;;
$let bad = (module R : S);;
