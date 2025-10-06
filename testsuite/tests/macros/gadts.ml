(* TEST
 expect;
*)

macro m : type a. (a, int) Type.eq expr -> a -> unit expr =
  fun p x ->
  << match $p with Type.Equal -> $(let _ = 1 + x in << () >>) >>
;;

[%%expect{|
macro m : ('a, int) Type.eq expr -> 'a -> unit expr = <fun>
|}]

macro m : type a. (a, int) Type.eq expr -> unit expr =
  fun p ->
  << match $p with Type.Equal -> () >>
;;

[%%expect{|
macro m : ('a, int) Type.eq expr -> unit expr = <fun>
|}]


(* TODO: we should eventually allow this *)
macro m : type a. (a, int) Type.eq expr -> a expr -> int expr =
 fun p x ->
  << match $p with Type.Equal -> $x >>
;;

[%%expect{|
macro m : ('a, int) Type.eq expr -> 'a expr -> int expr = <fun>
|}]
