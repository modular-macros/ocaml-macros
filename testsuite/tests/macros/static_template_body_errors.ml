(* TEST
 expect;
*)

(* What a static in a template functor's body may not name: the
   parameter's generators -- a generator has no environment to be
   given in an initialiser -- and the parameter's run-time members.
   Its static members are fine (static_template_body.ml).  The
   quotation case, a translator refusal, is
   static_template_body_quotes.ml. *)

module type S = sig $gen g : unit -> int expr  val w : int  $val v : int end

module F [X : S] = struct $let s = X.g () end
;;

[%%expect{|
module type S = sig $gen g : unit -> int expr val w : int $val v : int end
Line 3, characters 35-38:
3 | module F [X : S] = struct $let s = X.g () end
                                       ^^^
Error: The generator "X.g" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]

module G [X : S] = struct $let s = X.w + 1 end
;;

[%%expect{|
Line 1, characters 35-38:
1 | module G [X : S] = struct $let s = X.w + 1 end
                                       ^^^
Error: "X.w" is bound at run time, but this use is in compile-time code.
|}]

(* The toplevel does not yet support a static inside a module of a
   phrase, functor bodies included; batch does (static_template_body.ml,
   static_functor_body.ml). *)

module H [X : S] = struct $let s = X.v + 1 end
;;

[%%expect{|
Line 1, characters 26-42:
1 | module H [X : S] = struct $let s = X.v + 1 end
                              ^^^^^^^^^^^^^^^^
Error: A static binding is not yet supported inside a module of a
       toplevel phrase.
       Bind it directly at the top level, or put the module in a
       compiled unit.
|}]

module K (X : sig end) = struct $let s = 1 end
;;

[%%expect{|
Line 1, characters 32-42:
1 | module K (X : sig end) = struct $let s = 1 end
                                    ^^^^^^^^^^
Error: A static binding is not yet supported inside a module of a
       toplevel phrase.
       Bind it directly at the top level, or put the module in a
       compiled unit.
|}]
