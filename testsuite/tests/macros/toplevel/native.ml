(* TEST
 toplevel.opt;
*)

(* The native toplevel does not evaluate compile-time code: it refuses
   a phrase that has a top-level splice or a compile-time component. *)

let a = 1;;
let b = $(<< 2 >>);;
$let c = print_endline "evaluated"; 3;;
$let[..] d () = << 4 >>;;
module M = struct $let e = 5  let f = 6 end;;
let g = a + 1;;
