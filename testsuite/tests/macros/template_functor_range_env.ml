(* TEST
 expect;
*)

(* The range computation must see signature-local context: a module type
   bound earlier in the same signature, or in the signature of a functor
   parameter, must be expanded when the range of a later item is computed,
   so that level -1 components hidden behind such module types are still
   detected.  A genuinely abstract module type keeps range [0,0]; that is
   sound because instantiating one with a module type that has level -1
   components is rejected. *)

module type T = sig
  module type I = sig $gen m : unit -> int expr end
  module M : I
end
;;

[%%expect{|
module type T =
  sig module type I = sig $gen m : unit -> int expr end module M : I end
|}]

(* ------------------------------------------------------------------ *)
(* Macros hidden behind a signature-local module type                  *)
(* ------------------------------------------------------------------ *)

(* The $gen hidden behind the signature-local [I] is found in a plain
   functor argument. *)
module F (X : T) = struct end
;;

[%%expect{|
Line 1, characters 9-29:
1 | module F (X : T) = struct end
             ^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* Referencing the hidden $gen used to crash the compiler
   (Bytegen.comp_expr); the functor is rejected like the previous one. *)
module G (X : T) = struct let v = $(X.M.m ()) end
;;

[%%expect{|
Line 1, characters 9-49:
1 | module G (X : T) = struct let v = $(X.M.m ()) end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* The same, in a module type. *)
module type Bad = (X : T) -> sig end
;;

[%%expect{|
Line 1, characters 18-36:
1 | module type Bad = (X : T) -> sig end
                      ^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* A hidden $gen two signatures deep. *)
module type Deep = sig
  module type I = sig $gen m : unit -> int expr end
  module N : sig module M : I end
end
module K (X : Deep) = struct end
;;

[%%expect{|
module type Deep =
  sig
    module type I = sig $gen m : unit -> int expr end
    module N : sig module M : I end
  end
Line 5, characters 9-32:
5 | module K (X : Deep) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, template functors, or static bindings).
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* Body side: a $gen behind a local module type ascription inside a
   plain functor body. *)
module F (X : sig end) = struct
  module type I = sig $gen m : unit -> int expr end
  module M : I = struct $gen m () = << 1 >> end
end
;;

[%%expect{|
module F :
  (X : sig end) ->
    sig module type I = sig $gen m : unit -> int expr end module M : I end
|}]

(* ------------------------------------------------------------------ *)
(* Module types local to a functor parameter                           *)
(* ------------------------------------------------------------------ *)

module type WithLocalMT = sig
  module type I = sig $gen m : unit -> int expr end
end
;;

[%%expect{|
module type WithLocalMT =
  sig module type I = sig $gen m : unit -> int expr end end
|}]

(* Declaring the module type alone involves no level -1 component. *)
module H (X : WithLocalMT) = struct end
;;

[%%expect{|
module H : (X : WithLocalMT) -> sig end
|}]

(* A body component typed by the parameter's local module type does. *)
module type BadBody = (X : WithLocalMT) -> sig module M : X.I end
;;

[%%expect{|
module type BadBody = (X : WithLocalMT) -> sig module M : X.I end
|}]

module J (X : WithLocalMT) = struct
  module M : X.I = struct $gen m () = << 1 >> end
end
;;

[%%expect{|
module J : (X : WithLocalMT) -> sig module M : X.I end
|}]

(* ------------------------------------------------------------------ *)
(* The other checks built on the range computation                     *)
(* ------------------------------------------------------------------ *)

(* Packages. *)
let f (x : (module T)) = x
;;

[%%expect{|
Line 1, characters 19-20:
1 | let f (x : (module T)) = x
                       ^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators, template functors,
       or static bindings).  A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
|}]

(* Local module bindings. *)
let f () =
  let module L = struct
    module type I = sig $gen m : unit -> int expr end
    module M : I = struct $gen m () = << 1 >> end
  end in 1
;;

[%%expect{|
Lines 2-5, characters 6-5:
2 | ......module L = struct
3 |     module type I = sig $gen m : unit -> int expr end
4 |     module M : I = struct $gen m () = << 1 >> end
5 |   end.....
Error: A locally bound module cannot have compile-time components
       (generators, template functors, or static bindings).
       Bind it at the top level of a structure instead.
|}]

(* Abstract module type instantiation. *)
module type Abs = sig module type Inner end
module type BadInst = Abs with module type Inner = T
;;

[%%expect{|
module type Abs = sig module type Inner end
Line 2, characters 22-52:
2 | module type BadInst = Abs with module type Inner = T
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has compile-time components (generators, template
       functors, or static bindings).
|}]

(* ------------------------------------------------------------------ *)
(* Genuinely abstract module types keep range [0,0]                    *)
(* ------------------------------------------------------------------ *)

(* A component whose type is an abstract module type is fine: an
   abstract module type cannot be instantiated with a module type that
   has level -1 components, so it can hide no macros. *)
module type A
;;

[%%expect{|
module type A
|}]

module F (X : sig module type MT module M : MT end) = struct end
;;

[%%expect{|
module F : (X : sig module type MT module M : MT end) -> sig end
|}]

module type OkBody = (X : sig end) -> sig module type MT module M : MT end
;;

[%%expect{|
module type OkBody = (X : sig end) -> sig module type MT module M : MT end
|}]

module type OkParam = sig module type MT end
module type OkBody2 = (X : OkParam) -> sig module M : X.MT end
;;

[%%expect{|
module type OkParam = sig module type MT end
module type OkBody2 = (X : OkParam) -> sig module M : X.MT end
|}]

let f (x : (module A)) = x
;;

[%%expect{|
val f : (module A) -> (module A) = <fun>
|}]
