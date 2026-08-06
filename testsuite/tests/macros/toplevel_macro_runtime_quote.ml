(* TEST
 toplevel;
*)

(* A toplevel macro may quote a run-time binding of an earlier phrase:
   its quotations are closed over the run-time names they mention
   (Translcore.closing_toplevel_macro), and the returned code reaches the
   bindings through the toplevel value table. *)

let helper x = x + 1
;;

macro g () = << helper 5 >>
;;

let y = $(g ())
;;

(* Same phrase, and a binder alongside the quoted binding. *)
let twice x = x * 2
macro h () = << fun z -> z + twice 3 >>
let f = $(h ())
;;

f 10
;;

(* A macro calling another macro that quotes a run-time binding. *)
macro outer () = << 10 + $(g ()) >>
;;

let w = $(outer ())
;;

(* Quoted bindings are pinned when the macro is defined, so redefining
   [helper] -- even at another type -- does not re-aim the code [g]
   returns: capture is lexical and type-safe, as in batch compilation.
   See D-AUTO-7.md. *)
let helper x = x * 100
;;

let shadowed = $(g ())
;;

let helper x = x ^ "!"
;;

let still_an_int = $(g ()) + 1
;;

(* The pinned cell is the binding itself, so mutable state is shared. *)
let r = ref 10
;;

macro reads_r () = << !r >>
;;

incr r
;;

let sees_increment = $(reads_r ())
;;
