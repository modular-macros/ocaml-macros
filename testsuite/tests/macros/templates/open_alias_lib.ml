(* Compile-time components that another unit reaches through aliases. *)
$let k : int = 7
module T $(X : sig $val n : int end) = struct let v = $(Expr.int X.n) + 1 end
