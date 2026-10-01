(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* Code that mentions a variable of a quotation, taken out of the
   variable's scope by an exception, is detected when it is used. *)

exception Escape of int expr

let () =
  match << fun x -> $(raise (Escape << x >>)) >> with
  | _ -> print_endline "not raised"
  | exception Escape c ->
      match << $c + 1 >> with
      | _ -> print_endline "not detected"
      | exception Effect.Unhandled _ -> print_endline "detected"
