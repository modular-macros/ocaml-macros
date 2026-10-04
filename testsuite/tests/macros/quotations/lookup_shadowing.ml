(* TEST
 expect;
*)

(* An open serves its names at the levels of its module, and a name of
   the standard library is found for compile-time code through a module
   opened at run time.  A lexical binding shadows whatever its level, so
   that an include, which rebinds the names of its module lexically,
   shadows where the open did not. *)

module M = struct let print_endline (_ : int) = () end
;;
[%%expect{|
module M : sig val print_endline : int -> unit end
|}]

open M
let a = $(ignore (print_endline : string -> unit); << 1 >>)
;;
[%%expect{|
val a : int = 1
|}]

include M
let b = $(ignore (print_endline : string -> unit); << 1 >>)
;;
[%%expect{|
val print_endline : int -> unit = <fun>
Line 2, characters 18-31:
2 | let b = $(ignore (print_endline : string -> unit); << 1 >>)
                      ^^^^^^^^^^^^^
Error: "print_endline" is bound at run time, but this use is in compile-time code.
|}]

let print_string (_ : int) = ()
let c = $(ignore (print_string : string -> unit); << 1 >>)
;;
[%%expect{|
val print_string : int -> unit = <fun>
Line 2, characters 18-30:
2 | let c = $(ignore (print_string : string -> unit); << 1 >>)
                      ^^^^^^^^^^^^
Error: "print_string" is bound at run time, but this use is in compile-time code.
|}]
