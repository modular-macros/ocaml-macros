(* TEST
 expect;
*)

(* A plain functor's parameter may not contain compile-time
   components.  The range calculus counts a value at the level it
   serves, so a static binding ($val) is refused exactly as a
   generator is -- in a plain functor, in a mixed one, nested in a
   submodule of the parameter, and through a named module type.  A
   template functor's parameter admits them (static_template_body.ml). *)

module F (X : sig $val v : int end) = struct end
;;

[%%expect{|
Line 1, characters 9-48:
1 | module F (X : sig $val v : int end) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

module G (X : sig $val v : int end) = struct $gen m () = << 1 >> end
;;

[%%expect{|
Line 1, characters 9-68:
1 | module G (X : sig $val v : int end) = struct $gen m () = << 1 >> end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

module H (X : sig module N : sig $val v : int end end) = struct end
;;

[%%expect{|
Line 1, characters 9-67:
1 | module H (X : sig module N : sig $val v : int end end) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

module type S = sig $val v : int end
module K (X : S) = struct end
;;

[%%expect{|
module type S = sig $val v : int end
Line 2, characters 9-29:
2 | module K (X : S) = struct end
             ^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* The parameter's TYPE is what is tested, so an argument with a
   static is fine at a parameter signature that leaves it out:
   static_functor_body.ml (batch, since the toplevel does not yet
   admit a static inside a module of a phrase). *)
