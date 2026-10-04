(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A type extension in a unit with static bindings keeps its position
   in the two blocks of the unit, and its constructors are allocated at
   run time. *)

type t = ..
$let s = 1
type t += A | B of int
let x = $(Expr.int s)
exception E of int
$let u = s + 1
type t += C

let name = function A -> "A" | B n -> "B" ^ string_of_int n | C -> "C" | _ -> "?"

let () =
  print_endline (String.concat " " [name A; name (B x); name C]);
  try raise (E $(Expr.int u)) with E n -> print_int n; print_newline ()
