(* TEST
 expect;
*)

(* What a static in a template's body may not name: the
   parameter's generators -- a generator has no environment to be
   given in an initialiser -- and the parameter's run-time members.
   Its static members are fine (static_body.ml).  The
   quotation case, a translator refusal, is
   static_body_quotes.ml. *)

module type S = sig $val[..] g : unit -> int expr  val w : int  $val v : int end

module F $(X : S) = struct $let s = X.g () end
;;

[%%expect{|
module type S =
  sig $val[..] g : unit -> int expr val w : int $val v : int end
Line 3, characters 36-39:
3 | module F $(X : S) = struct $let s = X.g () end
                                        ^^^
Error: The generator "X.g" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]

module G $(X : S) = struct $let s = X.w + 1 end
;;

[%%expect{|
Line 1, characters 36-39:
1 | module G $(X : S) = struct $let s = X.w + 1 end
                                        ^^^
Error: "X.w" is bound at run time, but this use is in compile-time code.
|}]

(* The toplevel accepts a static binding inside a module of a phrase,
   functor bodies included, as batch compilation does
   (static_body.ml, static_functor_body.ml). *)

module H $(X : S) = struct $let s = X.v + 1 end
;;

[%%expect{|
module H : $(X : S) -> sig $val s : int end
|}]

module K (X : sig end) = struct $let s = 1 end
;;

[%%expect{|
module K : (X : sig end) -> sig $val s : int end
|}]
