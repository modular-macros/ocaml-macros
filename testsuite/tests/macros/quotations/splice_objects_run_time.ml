(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The splices of a quotation in run-time code are run-time code, and
   may make objects. *)

class c = object method m = print_endline "new c"; << 2 >> end

let a =
  << 1 + $(let o = object method m = print_endline "object"; << 1 >> end in
           o#m)
       + $((new c)#m) >>
