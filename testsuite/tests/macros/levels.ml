(* TEST
 expect;
*)

macro tardy x = <<x>>
;;

[%%expect{|
Line 1, characters 18-19:
1 | macro tardy x = <<x>>
                      ^
Error: Bad staging level. Bind level is -1, the expected level is 0. Mode is Q
|}]

macro hasty x = $x
;;

[%%expect{|
Line 1, characters 17-18:
1 | macro hasty x = $x
                     ^
Error: Bad staging level. Bind level is -1, the expected level is -2. Mode is S
|}]


macro timely x = x
;;

[%%expect{|
macro timely : 'a -> 'a = <fun>
|}]

macro timely2 x = << $x >>
;;

[%%expect{|
macro timely2 : 'a expr -> 'a expr = <fun>
|}]
