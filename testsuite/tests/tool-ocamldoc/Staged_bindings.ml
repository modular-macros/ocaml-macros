(* TEST
 ocamldoc with html;
*)

(** Static bindings and generators are documented with their keywords. *)

(** A value of run-time code *)
let run = 1

(** A static binding *)
$let static = 2

(** A generator *)
$let[..] generator x = << $x + 1 >>
