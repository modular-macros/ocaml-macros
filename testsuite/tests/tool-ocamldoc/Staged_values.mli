(* TEST
 {
   ocamldoc with html;
 }{
   ocamldoc with latex;
 }{
   ocamldoc with man;
 }
*)

(** Static bindings and generators are documented with their keywords. *)

val run : int
(** A value of run-time code *)

$val static : int
(** A static binding *)

$val[..] generator : int expr -> int expr
(** A generator *)
