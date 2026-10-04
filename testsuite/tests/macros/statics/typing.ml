(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A static binding is a value of compile-time code: its right-hand
   side is compile-time code, and it is used there. *)

$let n = 1 + 2
$let rec fact k = if k = 0 then 1 else k * fact (k - 1)
$let (a, b) = (10, 20)
$let table = List.init 4 (fun i -> i * i)
$let one = << 1 >>
$let plus c = << $c + 1 >>

let z = $(Expr.int (fact n + a + b + List.nth table 3))
let o = $(plus one)

(* A static binding is a component of its module. *)

module M = struct
  $let v = 41
  let w = 1
end

let y = $(Expr.int M.v) + M.w

module type S = sig
  $val v : int
  val w : int
end

module N : S = M

(* A signature may hide it. *)

module O : sig val w : int end = M

(* An opened module serves its static bindings to compile-time code. *)

open M

let x = $(Expr.int v) + w

(* So does a local open of its path. *)

let u = (let open M in $(Expr.int v)) + M.($(Expr.int v))
