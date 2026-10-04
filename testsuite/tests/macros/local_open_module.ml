(* TEST
 expect;
*)

(* A module that a local binding makes may not have compile-time
   components: it has no place in either world's block.  That holds of
   every local form but the open of a path, which makes no module. *)

module Lib = struct let v = 1  $let[..] g () = << 2 >> end
module F (X : sig end) = struct let v = 1  $let[..] g () = << 2 >> end
;;

[%%expect{|
module Lib : sig val v : int $val[..] g : unit -> int expr end
module F : (X : sig end) -> sig val v : int $val[..] g : unit -> int expr end
|}]

let ok = Lib.(v) + (let open Lib in v) + Lib.($(g ()))
;;

[%%expect{|
val ok : int = 4
|}]

let through_alias = let module N = Lib in N.v
;;

[%%expect{|
val through_alias : int = 1
|}]

let bad = let open struct $let[..] g () = << 1 >> end in 1
;;

[%%expect{|
Line 1, characters 14-53:
1 | let bad = let open struct $let[..] g () = << 1 >> end in 1
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

let bad = let open F (struct end) in v
;;

[%%expect{|
Line 1, characters 14-33:
1 | let bad = let open F (struct end) in v
                  ^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

let bad =
  let open (Lib : sig val v : int  $val[..] g : unit -> int expr end) in v
;;

[%%expect{|
Line 2, characters 6-69:
2 |   let open (Lib : sig val v : int  $val[..] g : unit -> int expr end) in v
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

(* An open in compile-time code serves what its module serves there. *)
module R = struct let v = 1 end
$let bad : int = let open R in v
;;

[%%expect{|
module R : sig val v : int end
Line 2, characters 31-32:
2 | $let bad : int = let open R in v
                                   ^
Error: "v" is bound at run time, but this use is in compile-time code.
|}]
