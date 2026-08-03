(* TEST
 expect;
*)

(* The mixed-functor typing surface (SPLITTING-FUNCTORS.md): mixed
   functor TYPES are legal everywhere, batch definitions compile
   (tests/macros/mixed_functor_basic.ml), the toplevel and recursive
   modules reject them for now, and every composed guard keeps
   firing. *)

module type A = sig val v : int end

(* Mixed functor types are accepted, and print. *)
module type T1 = (X : A) -> sig val w : int macro m : unit -> int expr end
[%%expect{|
module type A = sig val v : int end
module type T1 = (X : A) -> sig val w : int macro m : unit -> int expr end
|}]

(* A functor type producing a template functor type. *)
module type T2 = (X : A) -> [Y : A] sig val r : int end
[%%expect{|
module type T2 = (X : A) -> [Y : A] sig val r : int end
|}]

(* Curried and generative forms. *)
module type T3 = (X : A) -> (Y : A) -> sig macro m : unit -> int expr end
module type T4 = () -> sig macro m : unit -> int expr end
[%%expect{|
module type T3 = (X : A) (Y : A) -> sig macro m : unit -> int expr end
module type T4 = () -> sig macro m : unit -> int expr end
|}]

(* A mixed functor declared inside a signature. *)
module type S = sig
  module F : (X : A) -> sig macro m : unit -> int expr end
end
[%%expect{|
module type S =
  sig module F : (X : A) -> sig macro m : unit -> int expr end end
|}]

(* The ARGUMENT of a functor stays pure level 0: a macro-bearing
   parameter signature is still rejected ... *)
module type Bad1 = (X : sig macro g : unit -> int expr end) -> sig end
[%%expect{|
Line 1, characters 19-70:
1 | module type Bad1 = (X : sig macro g : unit -> int expr end) -> sig end
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain macros or template functors.
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* ... including a MIXED-FUNCTOR-typed parameter (higher order). *)
module type Bad2 = (F : T1) -> sig end
[%%expect{|
Line 1, characters 19-38:
1 | module type Bad2 = (F : T1) -> sig end
                       ^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain macros or template functors.
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* First-class modules reject mixed functor types. *)
let f (x : (module T1)) = x
[%%expect{|
Line 1, characters 19-21:
1 | let f (x : (module T1)) = x
                       ^^
Error: This module type cannot be used as a first-class module:
       it has macro or template functor components,
       which exist only at compile time.
|}]

(* Instantiating an abstract module type with a mixed functor type is
   rejected. *)
module type HasA = sig module type MT end
module type Inst = HasA with module type MT = T1
[%%expect{|
module type HasA = sig module type MT end
Line 2, characters 19-48:
2 | module type Inst = HasA with module type MT = T1
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has macro or template functor components.
|}]

(* A locally bound mixed functor is rejected by the local-module
   guard. *)
let g () =
  let module F = (functor (X : A) -> struct macro m () = << X.v >> end) in
  ()
[%%expect{|
Line 2, characters 6-71:
2 |   let module F = (functor (X : A) -> struct macro m () = << X.v >> end) in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have macro or template functor
       components. Bind it at the top level of a structure instead.
|}]

(* Recursive modules reject mixed functors (the plain
   recmodule-with-macros form below predates the guard and stays
   accepted). *)
module rec F : (X : A) -> sig macro m : unit -> int expr end =
  functor (X : A) -> struct macro m () = << X.v >> end
[%%expect{|
Lines 1-2, characters 0-54:
1 | module rec F : (X : A) -> sig macro m : unit -> int expr end =
2 |   functor (X : A) -> struct macro m () = << X.v >> end
Error: Mixed functors (functors producing structures with macro or
       template functor components) are not yet supported in
       recursive modules.
|}]

module rec M : sig macro g : unit -> int expr val v : int end = struct
  macro g () = << 1 >>
  let v = 2
end
let x = $(M.g ())
[%%expect{|
module rec M : sig macro g : unit -> int expr val v : int end
val x : int = 1
|}]

(* A mixed functor DEFINITION in the toplevel is rejected (batch
   definitions compile; tests/macros/mixed_functor_basic.ml). *)
module F (X : A) = struct
  let w = X.v * 2
  macro m () = << X.v + w >>
end
[%%expect{|
module F : (X : A) -> sig val w : int macro m : unit -> int expr end
|}]
