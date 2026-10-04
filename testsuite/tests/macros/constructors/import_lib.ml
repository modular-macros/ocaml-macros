exception $Stop of int
exception Failed of int
type t = ..
type t += $A | B
$let stop n = raise (Stop n)
let fail n = raise (Failed n)
$let[..] guarded body = << try $body with Failed n -> n >>
