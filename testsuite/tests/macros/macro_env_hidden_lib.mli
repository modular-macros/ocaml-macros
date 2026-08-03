(* [helper] is deliberately absent: a macro quotes it, and the environment
   captures its value rather than its name, so hiding it makes no
   difference to a caller. *)

macro gen : unit -> int expr
