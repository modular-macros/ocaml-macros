(* TEST
 readonly_files = "static_stage_runtime_items_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "\
   static_stage_runtime_items_lib.ml \
   static_stage_runtime_items.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Run-time-only items beside a macro, at the compile-time stage.  A
   constrained alias and a plain functor application used to be translated
   there for real, which put the other unit's run-time global into this
   unit's macro block and static program: the unit itself then failed to
   link its compile-time part, and so did every dependent.  They carry no
   compile-time content, so they take a dummy slot, as values and classes
   do.

   (An extension rebinding from another PROJECT unit -- [exception E =
   Lib.E] -- leaks the same way and is NOT fixed: it is indistinguishable
   here from a rebinding of Stdlib's, which the static program links and
   which macro bodies legitimately use.  The last two bindings below pin
   the cases that must keep working.) *)

macro g () = << 1 >>

module M1 : sig val f : unit -> int end = Static_stage_runtime_items_lib
module M2 = Static_stage_runtime_items_lib.Make (struct let n = 5 end)

(* A recursive group with no compile-time content is dummied alike. *)
module Plain (Y : sig val v : int end) = struct let w () = Y.v end
module rec R : sig val w : unit -> int end = Plain (struct let v = 3 end)

(* Extension items stay translated at both stages: a macro body may raise
   and match a locally declared exception or extension constructor, against
   this block's own constructor, and may use one rebound from Stdlib. *)
exception Local of int
type u = ..
type u += Ext of int
exception My_not_found = Not_found

macro caught () = (try raise (Local 4) with Local n -> Expr.int n)
macro matched () = (match Ext 6 with Ext n -> Expr.int n | _ -> Expr.int 0)
macro rebound () = (try raise My_not_found with Not_found -> Expr.int 8)

let z = $(g ())
let () =
  Printf.printf "%d %d %d %d %d %d %d\n"
    z (M1.f ()) M2.v (R.w ()) $(caught ()) $(matched ()) $(rebound ())
