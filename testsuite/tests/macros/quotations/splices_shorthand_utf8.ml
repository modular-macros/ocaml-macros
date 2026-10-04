(* TEST
 expect;
*)

(* The shorthand splices an identifier written as identifiers are, with
   letters outside ASCII too. *)

let xé = << 1 >>
let a = << $xé + 1 >>
;;
[%%expect{|
val xé : int expr = <code 1>
val a : int expr = <code (+ 1 1)>
|}]

let été = << 2 >>
let b = << $été + 1 >>
;;
[%%expect{|
val été : int expr = <code 2>
val b : int expr = <code (+ 2 1)>
|}]

module Été = struct let x = << 3 >> end
let c = << $Été.x + 1 >>
;;
[%%expect{|
module Été : sig val x : int expr end
val c : int expr = <code (+ 3 1)>
|}]

$let yé = << 10 >>
let n = $(<< $yé + 1 >>)
;;
[%%expect{|
$val yé : int expr = <code 10>
val n : int = 11
|}]
