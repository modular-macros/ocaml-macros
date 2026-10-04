(* TEST
 expect;
*)

(* The compile-time components of a module type are found through the
   module types bound in a signature, including a functor parameter's,
   and templates are among them. *)

module type T = sig
  module type I = $(X : sig end) -> sig end
  module M : I
end
;;
[%%expect{|
module type T =
  sig module type I = $(X : sig end) -> sig end module M : I end
|}]

module F (X : T) = struct end
;;
[%%expect{|
Line 1, characters 9-29:
1 | module F (X : T) = struct end
             ^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Use a template, whose parameter is written "$(X : S)",
       if the argument needs them.
|}]

module type Bad = (X : T) -> sig end
;;
[%%expect{|
Line 1, characters 18-36:
1 | module type Bad = (X : T) -> sig end
                      ^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Use a template, whose parameter is written "$(X : S)",
       if the argument needs them.
|}]

module type P = sig
  module type I = $(X : sig end) -> sig end
  module F (X : I) : sig end
end
;;
[%%expect{|
Line 3, characters 11-28:
3 |   module F (X : I) : sig end
               ^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Use a template, whose parameter is written "$(X : S)",
       if the argument needs them.
|}]

(* An abstract module type has no compile-time components. *)
module type Q = sig module type I  module F (X : I) : sig end end
;;
[%%expect{|
module type Q = sig module type I module F : (X : I) -> sig end end
|}]
