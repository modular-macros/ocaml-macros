(* TEST
 toplevel.opt;
*)

(* The native toplevel rejects compile-time constructs cleanly
   (bytecode-only-macros, LIMITATIONS.md): a splice, a template
   application and #static_use each fail their phrase, the first two
   with a located error, and the session survives.  Pinned for the
   toplevel round (TOPLEVEL.md phase 0), which must keep these
   rejections while lifting the bytecode toplevel's. *)

let ok = 1;;
let x = $( << 1 >> );;
module T[X : sig val v : int end] = struct let w = X.v end;;
module V = struct let v = 3 end;;
module M = T[V];;

(* #static_use names a compile-world archive for the static program to
   link.  There is no compile-time world here to link it into, so the
   directive refuses outright rather than accepting the name and failing
   later; the archive need not even exist for that answer. *)
#static_use "foo.cmo";;

Printf.printf "ok = %d\n" ok;;
