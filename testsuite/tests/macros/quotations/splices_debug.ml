(* TEST
 flags = "-g";
 {
   bytecode;
 }{
   native;
 }
*)

(* The term of a unit compiled with -g carries debug events, which the
   static program's copy of the term leaves out. *)

let f x = x + $(<< 3 >>)
let () = Printf.printf "%d\n" (f 1)
