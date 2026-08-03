(* TEST
 readonly_files = "template_face_nested_member_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "template_face_nested_member_lib.ml \
     template_face_nested_member.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "template_face_nested_member_lib.ml \
     template_face_nested_member.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Completing a template chain through a narrowed face whose result
   carries a nested template member (frex bug 5; the trail is in git history): merely
   initialising this unit exercises the run-time slots -- CdEva's
   prologue projects through Eva's captured-environment tuple -- and
   the splices exercise the coerced record side of the same
   components. *)

open Template_face_nested_member_lib

module M0 = struct
  type t = string
  macro unit : unit -> t = fun () -> ""
  macro (<*>) : t -> t -> t = fun a b -> a ^ b
end

module FE = F[M0]
module PSV = FE.PS[M0]
module CdEva = PSV.Eva[M0]

let () = print_endline "init ok"

macro lift : string -> string expr = fun s -> Expr.string s

let s = $(lift (PSV.sta "a"))
let e = $(lift (CdEva.eva "x" "y"))

let () = Printf.printf "s = %s\ne = %s\n" s e
