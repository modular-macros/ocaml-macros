(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* Labels have no level: records are built, read and updated in quoted
   code and in compile-time code alike. *)

type r = { a : int; mutable b : int }

let f =
  $(let s = { a = 1; b = 2 } in
    s.b <- s.a + s.b;
    << fun x ->
         let q = { a = x; b = $(Expr.int s.b) } in
         q.b <- q.b + 1;
         { q with a = q.a * 10 }.a + q.b >>)

let g = $(<< match { a = 5; b = 6 } with { a; b = 6 } -> a | { b; _ } -> b >>)

let () = Printf.printf "%d %d\n" (f 2) g
