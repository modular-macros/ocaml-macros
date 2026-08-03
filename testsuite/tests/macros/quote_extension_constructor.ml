(* TEST
 expect;
*)

(* Extension constructors across stages (NAME-RESOLUTION.md Part IV):
   a quoted mention is legal exactly when the constructor's declaration
   level equals the mention's ambient level -- then the world the built
   code runs in has an incarnation, and the quotation closes over the
   constructor like a value root.  Mentions with no incarnation in the
   built code's world are rejected statically (they used to crash the
   static program with SIGSEGV). *)

exception E
exception P of int
;;

[%%expect{|
exception E
exception P of int
|}]

(* A unit exception quoted from a splice (declared 0, quote ambient 0):
   the built code denotes the run world's incarnation, so a run-time
   handler catches it. *)
let ok1 = try $( << fun () -> raise E >> ) () with E -> 1
;;

[%%expect{|
val ok1 : int = 1
|}]

(* Same from a macro, with a payload, and a quoted MATCH. *)
macro m1 () = << fun () -> try raise (P 5) with P n -> n >>
let ok2 = $(m1 ()) ()
;;

[%%expect{|
macro m1 : unit -> (unit -> int) expr = <fun>
val ok2 : int = 5
|}]

(* An extension constructor of an extensible type. *)
type t = ..
type t += A of int
macro m2 () = << fun () -> A 3 >>
let ok3 = match $(m2 ()) () with A n -> n | _ -> 0
;;

[%%expect{|
type t = ..
type t += A of int
macro m2 : unit -> (unit -> t) expr = <fun>
val ok3 : int = 3
|}]

(* [%extension_constructor] closes over the same root. *)
let ok4 =
  Obj.repr [%extension_constructor E]
  == Obj.repr $( << [%extension_constructor E] >> )
;;

[%%expect{|
val ok4 : bool = true
|}]

(* A quotation's own [let exception] is its own world's: fine. *)
macro m3 () = << let exception Y in fun () -> raise Y >>
let ok5 = try $(m3 ()) () with _ -> 6
;;

[%%expect{|
macro m3 : unit -> (unit -> 'a) expr = <fun>
val ok5 : int = 6
|}]

(* A macro body's [let exception] quoted (declared -1, ambient 0): the
   run world has no incarnation.  Rejected. *)
macro bad1 () = let exception X in << fun () -> raise X >>
;;

[%%expect{|
Line 1, characters 54-55:
1 | macro bad1 () = let exception X in << fun () -> raise X >>
                                                          ^
Error: The extension constructor "X" cannot be used inside this
       quotation: the code a quotation builds runs in another program,
       which has no such constructor, so the quotation would capture
       this one's.
|}]

(* A quotation not realigned by a splice (ambient 1): no entry at that
   level, consistent with the level narrowing of values. *)
let bad2 = << E >>
;;

[%%expect{|
Line 1, characters 14-15:
1 | let bad2 = << E >>
                  ^
Error: The extension constructor "E" cannot be used inside this
       quotation: the code a quotation builds runs in another program,
       which has no such constructor, so the quotation would capture
       this one's.
|}]

(* Predefined constructors are fixed by the runtime: allowed anywhere. *)
let ok6 = $( << fun () -> try raise Not_found with Not_found -> 4 >> ) ()
;;

[%%expect{|
val ok6 : int = 4
|}]

(* A mention through a global root (the stdlib) re-resolves at link
   time in the consuming world: allowed. *)
let ok7 = $( << fun () -> try raise Stdlib.Exit with Stdlib.Exit -> 5 >> ) ()
;;

[%%expect{|
val ok7 : int = 5
|}]

(* A REBIND under a quotation follows the same level rule: at the
   matching level the built module's alias denotes the consuming
   world's incarnation -- the closing analysis collects rebind
   right-hand sides -- so identity is preserved even through a packed
   escape. *)
module type S = sig exception F end
let ok9 =
  let m = $( << (module struct exception F = E end : S) >> ) in
  let module M = (val m) in
  (try raise M.F with E -> 9)
;;

[%%expect{|
module type S = sig exception F end
val ok9 : int = 9
|}]

let ok10 = $( << fun () -> let exception F = E in raise F >> )
let ok11 = try ok10 () with E -> 11
;;

[%%expect{|
val ok10 : unit -> 'a = <fun>
val ok11 : int = 11
|}]

(* A rebind of a constructor whose world has no incarnation stays
   rejected. *)
macro bad3 () =
  let exception X in
  << (module struct exception F = X end : S) >>
;;

[%%expect{|
Line 3, characters 34-35:
3 |   << (module struct exception F = X end : S) >>
                                      ^
Error: Bad staging mode. The extension constructor "X" cannot be rebound inside this quotation: the world the built code runs in has no incarnation of it, so the rebinding would capture the building world's allocation.
|}]

(* An antiquote body resets the mode: an exception carrying a payload
   through compile-time control flow stays legal (the
   scope_extrusion_exception.ml pattern). *)
exception Carry of int expr
macro m4 () =
  try let _ = << fun x -> $(raise (Carry << x >>)) >> in << 0 >>
  with Carry _ -> << 6 >>
let ok8 = $(m4 ())
;;

[%%expect{|
exception Carry of int expr
macro m4 : unit -> int expr = <fun>
val ok8 : int = 6
|}]
