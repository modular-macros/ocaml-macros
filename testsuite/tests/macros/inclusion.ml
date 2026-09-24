(* TEST
 expect;
*)

module M :
sig
  val x : unit -> unit
end =
struct
  $gen x u = u
end
;;

[%%expect{|
Lines 5-7, characters 0-3:
5 | struct
6 |   $gen x u = u
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig $gen x : 'a -> 'a end
       is not included in
         sig val x : unit -> unit end
       Values do not match:
         $gen x : 'a -> 'a
       is not included in
         val x : unit -> unit
       One is a generator ($gen) and the other an ordinary value
|}]

module M :
sig
  $gen x : unit -> unit
end =
struct
  let x u = u
end
;;

[%%expect{|
Lines 5-7, characters 0-3:
5 | struct
6 |   let x u = u
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : 'a -> 'a end
       is not included in
         sig $gen x : unit -> unit end
       Values do not match:
         val x : 'a -> 'a
       is not included in
         $gen x : unit -> unit
       One is a generator ($gen) and the other an ordinary value
|}]


module type S = module type of struct
  module M :
  sig
    $gen x : unit -> unit
  end =
  struct
    $gen x u = u
  end
end
;;

[%%expect{|
module type S = sig module M : sig $gen x : unit -> unit end end
|}]
