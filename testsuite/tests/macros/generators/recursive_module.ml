(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A generator in a recursive module, called from a splice in the body
   of the module and from another module *)

module rec A : sig
  val v : int
  $val[..] g : int expr -> int expr
  val w : int
end = struct
  let v = 5
  $let[..] g c = << $c + v >>
  let w = $(g << 1 >>)
end

let x = $(A.g << 10 >>)
let () = Printf.printf "%d %d\n" A.w x
