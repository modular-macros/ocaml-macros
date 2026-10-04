(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The compile-time block of a functor's body is shared by all its
   applications, a generative functor's included: the static state of
   the body is one.  The run-time state is each application's own. *)

module Gen () = struct
  $let count = ref 0
  $let[..] next () = incr count; Expr.int !count
  let state = ref 0
  $let[..] bump () = << incr state; !state >>
end

module G1 = Gen ()
module G2 = Gen ()

let a = $(G1.next ())
let b = $(G2.next ())
let c = $(G1.bump ())
let d = $(G2.bump ())

let () = Printf.printf "%d %d %d %d\n" a b c d
