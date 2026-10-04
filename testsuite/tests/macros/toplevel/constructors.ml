(* TEST
 toplevel;
*)

(* Compile-time constructors in the toplevel *)

exception $Stop of int;;
$let found = try raise (Stop 1) with Stop n -> n;;
Stop 2;;
type t = ..;;
type t += $A | B;;
$let a = match A with A -> 3 | _ -> 0;;
let b = match B with B -> 4 | _ -> 0;;
exception $Halt = Stop;;
$let rebound = try raise (Halt 5) with Stop n -> n;;
module M = struct exception $Inner end;;
$let inner = try raise M.Inner with M.Inner -> 6;;
let spliced = $(try raise (Stop 7) with Stop n -> Expr.int n);;
$(raise (Stop 8));;
