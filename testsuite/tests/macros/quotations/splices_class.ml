(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A top-level splice may stand in the method of a class. *)

class c = object
  val v = 2
  method m = v + $(<< 1 >>)
end

let () = print_int (new c)#m; print_newline ()
