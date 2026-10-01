(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A splice in a quotation may perform an effect that a handler outside
   the quotation handles. *)

open Effect
open Effect.Deep

type _ Effect.t += Ask : int Effect.t

let build () = << fun x -> x + $(Expr.int (perform Ask)) >>

let () =
  let c =
    try_with build ()
      { effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Ask -> Some (fun (k : (a, _) continuation) -> continue k 41)
          | _ -> None }
  in
  ignore (c : (int -> int) expr);
  print_endline "built"
