(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* Code that mentions a variable of a quotation may be used only in
   the scope of that variable. *)

let extruded = ref << 0 >>
let _ = << fun x -> $(extruded := << x >>; << x >>) >>

let () =
  match << $(!extruded) + 1 >> with
  | _ -> print_endline "not detected"
  | exception Effect.Unhandled _ -> print_endline "detected"

(* It may pass through a reference, as long as it is used in scope. *)

let _ = << fun x -> $(let r = ref << x >> in << $(!r) + 1 >>) >>

let () = print_endline "done"
