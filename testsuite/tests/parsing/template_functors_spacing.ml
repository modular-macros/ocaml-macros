(* TEST
 toplevel;
*)

(* A template's parameter and argument open with "$(", one token, as a
   splice does.  The closing parenthesis is the ordinary one, so
   adjacent delimiters need no spacing discipline: "$()", "))" and
   ")$(" all lex as intended.

   The applications below use a functor F, so the phrases that
   parse are rejected by the type checker with a kind mismatch: the
   typing error is what shows that parsing succeeded.

   The dollar is an operator character.  It must be glued to the
   parenthesis, and an operator character before it joins it: after
   "=" or "->" the opening needs a space. *)

module type S = sig end
module type A = sig end
module type B = sig end
module type C = sig end
module F(X : S) = struct end
;;

(* Empty: "$()" is two tokens. *)
module G$() = struct end
;;

(* "))": a nested application closes with no space. *)
module M = F$(F$(struct end))
;;

(* ")$(": adjacent argument lists. *)
module M = F$(struct end)$(struct end)
;;

(* Nested template types, and a parameter that is not named. *)
module type T = $($(A) -> B) -> C
;;

(* One arrow may follow several parameters, of either kind. *)
module type T = $(X : A) (Y : B) $() -> C
;;

(* A space between a name and the opening is allowed. *)
module M = F $(struct end)
;;

(* A dollar apart from its parenthesis is an operator ... *)
module type T = $ (X : A) -> B
;;

(* ... and so is one that follows an operator character. *)
module type T =$(X : A) -> B
;;
module type T = (Y : A) ->$(X : A) -> B
;;

(* The short form glues the dollar to a name, and is subject to the
   same rules. *)
module type T = $A -> $B -> C
;;
module type T = $ A -> B
;;
module type T =$A -> B
;;

(* The short form takes a name that is not capitalised.  "$let" and
   "$val" are keywords, and neither names a module type. *)
module type s = sig end
module N = struct module type s = sig end end
;;
module type T = $s -> $N.s -> C
;;
module type T = $let -> C
;;

(* Square brackets do not delimit a template's parameter or argument. *)
module M = F[struct end]
;;
module type T = [X : A] B
;;
