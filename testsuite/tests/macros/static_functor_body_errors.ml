(* TEST
 expect;
*)

(* A static in a plain functor's body may not read the parameter's
   values: the parameter is bound at level 0 and the initialiser is
   compile-time code.  (Its types are fine: static_functor_body.ml.) *)

module F (X : sig val v : int end) = struct
  $let s = X.v + 1
end
;;

[%%expect{|
Line 2, characters 11-14:
2 |   $let s = X.v + 1
               ^^^
Error: "X.v" is bound at run time, but this use is in compile-time code.
|}]

(* Nor a parameter value reached through a nested module of the
   parameter. *)

module G (X : sig module N : sig val v : int end end) = struct
  $let s = X.N.v + 1
end
;;

[%%expect{|
Line 2, characters 11-16:
2 |   $let s = X.N.v + 1
               ^^^^^
Error: "X.N.v" is bound at run time, but this use is in compile-time code.
|}]
