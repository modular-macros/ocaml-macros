(* TEST
 toplevel;
*)

(* An anonymous phrase structure has no name, so the toplevel has no
   record key under which to keep a compile-time part for it, and
   toplevel_mixed_functor_forms.ml pins the located error a MIXED FUNCTOR
   in one gets.  The two remaining anonymous shapes end differently
   (LIMITATIONS.md, toplevel-mixed-functor-corners).

   Plain macros are accepted silently: nothing outside the structure can
   name them either, so losing them costs nothing and no diagnostic is
   warranted.  The structure prints nothing at all, and the session
   carries on.

   A TEMPLATE FUNCTOR is rejected, but by the translator's generic
   message rather than by a located toplevel error -- the same definition
   in a NAMED phrase structure works (toplevel_template_functors.ml) and
   both forms compile in batch.  It is recorded as the corner it is
   rather than smoothed over, and the phrase after it shows the session
   survives. *)

module _ = struct macro q () = << 1 >> end;;

print_endline "after-macro";;

module type S = sig val v : int end;;

module _ = struct module F [X : S] = struct let w = X.v end end;;

print_endline "after-template";;
