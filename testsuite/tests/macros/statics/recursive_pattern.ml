(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* In the body of a recursive module, the static bindings that follow a
   static binding by a pattern are in scope of the body's splices. *)

module rec A : sig val c : int end = struct
  $let (a, _z) = (1, 2)
  $let b = a + 1
  let c = $(Expr.int b)
end

module rec B : sig val c : int end = struct
  $let [@warning "-8"] (Some a) = Some 1
  $let b = a + 1
  let c = $(Expr.int b)
end

(* So are those of a recursive module in the body of another. *)

module rec D : sig val c : int end = struct
  module rec E : sig val d : int end = struct
    $let [@warning "-8"] (Some x) = Some 10
    let d = $(Expr.int x)
  end
  $let (y, _) = (5, 0)
  let c = E.d + $(Expr.int y)
end

let () = Printf.printf "%d %d %d\n" A.c B.c D.c
