(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* Code that uses an equation outside the arm that learned it is
   detected at run time too, when it is used in a quotation of run-time
   code. *)

type (_, _) eql = Refl : ('a, 'a) eql

let stash : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell : b expr option ref = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    Option.get !cell

let () =
  match << ignore $(stash << Obj.magic 0 >> << 1 >>) >> with
  | _ -> print_endline "not detected"
  | exception Effect.Unhandled _ -> print_endline "detected"
