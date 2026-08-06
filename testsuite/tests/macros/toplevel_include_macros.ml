(* TEST
 toplevel;
*)

(* Macros bound by include (and open) in toplevel phrases.  The include
   argument is translated in its own right by the phrase's compile-time
   part, which collects the splices inside it where the include's macros
   are in scope, and binds the include's compile-time components -- and
   only those -- into the toplevel value table before any later item's
   splice thunks are stored. *)

(* A macro and a splice calling it, inside one included structure. *)
include struct macro two () = << 2 >> let z = $(two ()) end
;;

(* A later phrase using both components. *)
let w = $(two ()) + z
;;

(* The macro in one item, the splice in a later item of the same
   phrase. *)
include struct macro three () = << 3 >> end let t = $(three ())
;;

(* Nested includes. *)
include struct include struct macro four () = << 4 >> let f4 = $(four ()) end end
;;

(* An include alongside other items of the phrase. *)
let a = 10 include struct macro five () = << 5 >> let f5 = $(five ()) end let b = f5 + a
;;

(* A run-time component before the macro: the compile-time block keeps a
   dummy slot for it, so the macro's position agrees with the run-time
   layout. *)
include struct exception E macro six () = << 6 >> let s = $(six ()) end
;;

(* include of a named module with macros, and a splice in the same
   phrase. *)
module M = struct macro seven () = << 7 >> end include M let s7 = $(seven ())
;;

(* A compile-time projection out of a module with a run-time component
   before the macro, in the module's own phrase. *)
module N = struct let unused = 0 macro eight () = << 8 >> end let e = $(N.eight ())
;;

(* open struct .. end shares the path. *)
open struct macro nine () = << 9 >> let n9 = $(nine ()) end
;;

n9
;;
