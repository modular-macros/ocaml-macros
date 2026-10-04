exception $Stop of int
exception Failed of int
type t = ..
type t += $A | B
$val stop : int -> 'a
val fail : int -> 'a
$val[..] guarded : int expr -> int expr
