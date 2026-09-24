(* TEST
 expect;
*)

(* A $gen's compile-time function occupies one position in the module's
   $gen block, so its pattern must bind exactly one name.  Typing used to
   check only that the right-hand side was a function; a pattern binding none
   or several reached the translator, which assumed one and called
   Misc.fatal_error -- a compiler crash in both back ends, and silently
   accepted in the toplevel. *)

$gen _ = fun () -> << 1 >>
;;

[%%expect{|
Line 1, characters 5-6:
1 | $gen _ = fun () -> << 1 >>
         ^
Error: A generator must bind exactly one name.
       Its compile-time function occupies that name's position in the
       module's compile-time block, so a pattern binding none, or
       several, has nowhere to go.
|}]

$gen (m as n) = fun () -> << 1 >>
;;

[%%expect{|
Line 1, characters 5-13:
1 | $gen (m as n) = fun () -> << 1 >>
         ^^^^^^^^
Error: A generator must bind exactly one name.
       Its compile-time function occupies that name's position in the
       module's compile-time block, so a pattern binding none, or
       several, has nowhere to go.
|}]

(* One name is one name, however it is written. *)

$gen (m : unit -> int expr) = fun () -> << 1 >>
;;

[%%expect{|
$gen m : unit -> int expr = <fun>
|}]

$gen (_ as n) = fun () -> << 2 >>
;;

[%%expect{|
$gen n : unit -> int expr = <fun>
|}]

let () = Printf.printf "%d %d\n" $(m ()) $(n ())
;;

[%%expect{|
|}]
