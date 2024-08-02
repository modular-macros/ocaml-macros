(* TEST
 expect;
*)

module M :
sig
  val x : unit -> unit
end =
struct
  macro x u = u
end
;;

[%%expect{|
Lines 5-7, characters 0-3:
5 | struct
6 |   macro x u = u
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig macro x : 'a -> 'a end
       is not included in
         sig val x : unit -> unit end
       Values do not match:
         macro x : 'a -> 'a
       is not included in
         val x : unit -> unit
       The staging levels do not match
|}]

module M :
sig
  macro x : unit -> unit
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
         sig macro x : unit -> unit end
       Values do not match:
         val x : 'a -> 'a
       is not included in
         macro x : unit -> unit
       The staging levels do not match
|}]


module type S = module type of struct
  module M :
  sig
    macro x : unit -> unit
  end =
  struct
    macro x u = u
  end
end
;;

[%%expect{|
module type S = sig module M : sig macro x : unit -> unit end end
|}]
