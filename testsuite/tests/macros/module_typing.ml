(* TEST
 expect;
*)

module M(X: sig macro m : int -> int expr end) =
struct
  let x = $(X.m 0)
end

[%%expect{|
File "_none_", line 1:
ok
|}]
