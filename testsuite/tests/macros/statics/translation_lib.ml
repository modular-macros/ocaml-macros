(* Static bindings among the run-time items of a unit, at each kind of
   item and under a signature that hides some of them. *)

let a = 1
$let x = print_endline "x"; 10
$let rec fact n = if n = 0 then 1 else n * fact (n - 1)
let b = a + 1

module Sub = struct
  let c = 3
  $let y = fact 3
  let d = c + 1
end

module Statics = struct
  $let y = print_endline "Statics.y"; x + Sub.y
  let e = 5
end

module Plain = struct
  let g = 6
  let v = 7
end

module Hidden : sig val a : int end = struct
  $let s = print_endline "Hidden.s"; 0
  let a = 11
end

module Alias = Statics
module Restricted : sig val v : int end = Plain
module Ints = Set.Make (Int)

include struct
  $let z = Statics.y + Alias.y
  let h = 8
end

open struct
  $let w = z + 1
  let i = 9
end

exception E
class k = object method m = 10 end
external id : 'a -> 'a = "%identity"

$let code = << Stdlib.succ 1 >>
$let () = Printf.printf "x = %d, z = %d, w = %d\n" x z w

let f n = n + b + Sub.d + Plain.g + Hidden.a + h + i + (new k)#m + id 0
