(* TEST
 toplevel;
*)

(* Toplevel items whose stage handling was wrong.

   A class has no compile-time content, so a phrase's compile-time part must
   not translate it.  Translating it evaluated the class twice -- its
   initialisation effects with it -- and, where it mentioned another binding
   of the same phrase group, reached for a run-time value the compile-time
   world has not got. *)

macro m () = << 3 >>
;;

class c = let () = print_endline "class initialiser" in object method v = 1 end
let x = $(m ())
;;

(* Once, not twice. *)

(* A class mentioning a binding of the same phrase group, beside a splice. *)
let base = 39
class d = object method v = base end
let y = $(m ())
;;

let () = Printf.printf "%d\n" ((new d)#v + y)
;;

(* A splice inside a class item. *)
class e = object method v = $(m ()) + 39 end
;;

let () = Printf.printf "%d\n" (new e)#v
;;

(* Rebinding a module whose compile-time part is held under the name it was
   bound to: refused, rather than segfaulting at the point of use.  A plain
   alias carries the key and still works. *)

module G () = struct macro g () = << 7 >> end
;;

module A = G ()
;;

module Alias = A
let () = Printf.printf "%d\n" $(Alias.g ())
;;

module Ascribed = (A : sig macro g : unit -> int expr end)
;;

module Included = struct include A end
;;

(* A top-level splice inside a recursive module binding: refused, rather than
   reaching Bytegen with the thunk's binder out of scope.  (Batch compilation
   supports it; see tests/macros/modules.ml.) *)

module rec R : sig val v : int end = struct let v = $(m ()) end
;;
