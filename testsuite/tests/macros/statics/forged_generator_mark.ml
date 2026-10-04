(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* An attribute does not make a value a generator: [f] is an ordinary
   function. *)

let f = fun x -> x + 1 [@@ocaml.macro]
let () = print_int (f 41); print_newline ()
