(* TEST
 toplevel;
*)

(* Template functors in the toplevel.  The toplevel is itself a
   compile-time world process: a definition phrase stores F's component
   function and its environment tuple in the value table; an application
   phrase evaluates the destructured component call in its compile-time
   part -- the record becoming M's compile-time meaning for later
   phrases -- and the instantiated fragment's code fills the hole at M's
   position in the run-time term through the phrase's splice-slot
   channel. *)

module type S = sig val base : int macro gen : unit -> int expr end
;;

let scale = 3
;;

(* A definition: level 0 code over X and an enclosing toplevel binding; a
   body macro quoting a body binding; a body splice with an effect,
   calling the body macro and the argument's macro; per-instantiation
   state. *)
module F[X : S] = struct
  let v = X.base * scale
  macro m () = << v + 1 >>
  let w = $( (print_endline "[body splice]"; m ()) ) + $(X.gen ())
  let r = ref v
end
;;

module V = struct
  let base = 10
  macro gen () = << 4 >>
end
;;

module V2 = struct
  let base = 100
  macro gen () = << 7 >>
end
;;

(* The body splice's effect happens once per application, when the
   application phrase's compile-time part runs. *)
module M = F[V]
;;

module M2 = F[V2]
;;

(* Per-instantiation values. *)
(M.v, M.w, M2.v, M2.w)
;;

(* Per-instantiation state. *)
let () = M.r := !M.r + 1
;;

(!M.r, !M2.r)
;;

(* The instantiated macro, called from a later phrase: the function comes
   from M's record, the environment from M's fragment block. *)
let y = $(M.m ()) + $(M2.m ())
;;

(* Applications and consuming splices in one phrase; the application's
   compile-time part evaluates at its item, splices at their holes. *)
module Ma = F[V] let s = $(Ma.m ())
;;

s
;;

(* A unit parameter. *)
module G[] = struct let u = $( << 5 >> ) end
;;

module MG = G[]
;;

MG.u
;;

(* Curried: a partial application's record is the inner component
   function, its run-time slot the extended context; both cross
   phrases. *)
module type S2 = sig val b : int macro h : int expr -> int expr end
;;

module C[X : S] [Y : S2] = struct
  let cv = X.base + Y.b + scale
  macro k () = << $(Y.h (X.gen ())) + cv >>
  let cw = $(k ())
end
;;

module H = C[V]
;;

module W = struct let b = 7 macro h c = << $c * 10 >> end
;;

module MC = H[W]
;;

(MC.cv, MC.cw, $(MC.k ()))
;;

(* An applied module as the next argument. *)
module type SM = sig macro gen : unit -> int expr end
;;

module Inc[X : SM] = struct
  macro gen () = << $(X.gen ()) + 1 >>
end
;;

module I1 = Inc[V]
;;

module I2 = Inc[I1]
;;

$(I2.gen ())
;;

(* An application inside a template functor body (Example E), at the
   toplevel. *)
module Use[X : SM] = struct
  module M0 = Inc[X]
  let a = $(M0.gen ()) * 2
end
;;

module U = Use[V]
;;

(U.a, $(U.M0.gen ()))
;;

(* The applied functor may be a literal template functor, and the
   argument a literal structure -- including one with macro
   components.  Both are accepted; the outputs below are the whole
   claim. *)
module B = (functor [X : SM] -> struct let u = 1 end)[V]
;;

module B = Inc[struct macro gen () = << 5 >> end]
;;

(* An anonymous application is evaluated and binds nothing. *)
module _ = Inc[V]
;;

(* A mixed-content argument (run-time and macro components) defined by
   the application's own phrase: since the stage -1 layout-parity fix
   the static block shares the full layout, so this works (see
   toplevel_same_phrase_template_arg.ml for the dedicated cases). *)
module Mixed = struct let base = 1 macro gen () = << 2 >> end
module Bad = F[Mixed]
;;

(* Template definitions and applications nested in plain structures stay
   unsupported. *)
module D = struct module N[X : SM] = struct let v = 1 end end
;;

module E = struct module N = Inc[V] end
;;
