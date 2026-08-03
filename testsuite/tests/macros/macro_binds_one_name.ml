(* TEST
 expect;
*)

(* A macro's compile-time function occupies one position in the module's
   macro block, so its pattern must bind exactly one name.  Typing used to
   check only that the right-hand side was a function; a pattern binding none
   or several reached the translator, which assumed one and called
   Misc.fatal_error -- a compiler crash in both back ends, and silently
   accepted in the toplevel. *)

macro _ = fun () -> << 1 >>
;;

[%%expect{|
Line 1, characters 6-7:
1 | macro _ = fun () -> << 1 >>
          ^
Error: A macro must bind exactly one name.
       Its compile-time function occupies that name's position in the
       module's macro block, so a pattern binding none, or several, has
       nowhere to go.
|}]

macro (m as n) = fun () -> << 1 >>
;;

[%%expect{|
Line 1, characters 6-14:
1 | macro (m as n) = fun () -> << 1 >>
          ^^^^^^^^
Error: A macro must bind exactly one name.
       Its compile-time function occupies that name's position in the
       module's macro block, so a pattern binding none, or several, has
       nowhere to go.
|}]

(* One name is one name, however it is written. *)

macro (m : unit -> int expr) = fun () -> << 1 >>
;;

[%%expect{|
macro m : unit -> int expr = <fun>
|}]

macro (_ as n) = fun () -> << 2 >>
;;

[%%expect{|
macro n : unit -> int expr = <fun>
|}]

let () = Printf.printf "%d %d\n" $(m ()) $(n ())
;;

[%%expect{|
|}]
