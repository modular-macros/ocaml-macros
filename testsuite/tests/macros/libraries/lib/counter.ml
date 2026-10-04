type t = A | B
let name = function A -> "A" | B -> "B"
let count = ref 0
let next () = incr count; !count
