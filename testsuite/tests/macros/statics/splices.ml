(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The splices of a unit may use its static bindings, wherever in the
   unit they are bound. *)

$let x = 10
$let rec fact n = if n = 0 then 1 else n * fact (n - 1)
let a = $(Expr.int (x + 1))

module M = struct
  $let y = fact 4
  let b = $(Expr.int (y + x))
end
let c = $(Expr.int M.y)

module Hidden : sig val d : int end = struct
  $let s = 3
  let d = $(Expr.int (s + x))
end

include struct
  $let z = 5
  let e = $(Expr.int z)
end

open struct
  $let w = 6
end
let f = $(Expr.int (w + z))

module rec R : sig
  $val k : int -> int
  val g : int -> int
end = struct
  $let k n = n + 1
  let g n = $(Expr.int (k 1)) + S.h n
end
and S : sig val h : int -> int end = struct
  let h n = n * 2
end
let h = $(Expr.int (R.k 2))

(* A static binding may hold code. *)

$let code = << Stdlib.succ 1 >>
let i = $code + $(<< $code + 1 >>)

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d\n"
    a M.b c Hidden.d e f (R.g 1) h i
