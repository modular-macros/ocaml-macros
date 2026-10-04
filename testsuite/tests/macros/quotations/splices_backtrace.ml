(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 {
   bytecode;
 }{
   native;
 }
*)

(* A unit with a top-level splice keeps its debug events: a backtrace
   through its functions gives their names and locations. *)

exception E

let[@inline never] g x = if x > $(<< 0 >>) then raise E else x
let[@inline never] f x = g (x + 1) + 1

let () =
  Printexc.record_backtrace true;
  match f 1 with
  | _ -> ()
  | exception E -> Printexc.print_backtrace stdout
