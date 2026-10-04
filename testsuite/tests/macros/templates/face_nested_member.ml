(* TEST
 readonly_files = "face_nested_member_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "face_nested_member_lib.ml \
     face_nested_member.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "face_nested_member_lib.ml \
     face_nested_member.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A template chain completed through a narrowed face whose result
   carries a nested template member: initialising this unit exercises
   the run-time slots -- CdEva's prologue projects through Eva's
   environment tuple -- and the splices the coerced record side of the
   same components. *)

open Face_nested_member_lib

module M0 = struct
  type t = string
  $let[..] unit : unit -> t = fun () -> ""
  $let[..] (<*>) : t -> t -> t = fun a b -> a ^ b
end

module FE = F$(M0)
module PSV = FE.PS$(M0)
module CdEva = PSV.Eva$(M0)

let () = print_endline "init ok"

$let[..] lift : string -> string expr = fun s -> Expr.string s

let s = $(lift (PSV.sta "a"))
let e = $(lift (CdEva.eva "x" "y"))

let () = Printf.printf "s = %s\ne = %s\n" s e
