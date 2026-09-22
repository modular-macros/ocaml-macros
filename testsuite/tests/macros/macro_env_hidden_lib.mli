(* [helper] is deliberately absent: a $gen quotes it, and the environment
   captures its value rather than its name, so hiding it makes no
   difference to a caller. *)

$gen gen : unit -> int expr
