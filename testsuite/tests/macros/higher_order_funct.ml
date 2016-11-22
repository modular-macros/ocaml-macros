module type S = sig
 macro x : int expr
 val y : int
end
;;

module Foo (F : functor (X : S) -> S) = struct
 module One = struct
   let x = 1
   let y = 10
   macro x = <<x>>
 end
 module Result = F(One)
end
;;

module Add1 (X : sig macro x : int expr end) = struct
 let y = 42 + 1
 let add = (+)
 macro x = << add $(X.x) 1 >>
 static blah = "blah"
 let blah' = "blah'"
end
;;

module Bar = Foo(Add1);;

let () = print_int $(Bar.Result.x); print_newline ();;
let () = print_int Bar.Result.y; print_newline ();;
