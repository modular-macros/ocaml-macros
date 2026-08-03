(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* What a macro's environment captures.  [macro_env_local] covers the
   plain shapes -- one function, several bindings, an empty environment;
   this file walks the rest of the "what is captured" axis, and checks
   VALUES rather than mere compilation, since a mis-indexed environment
   reads a neighbouring slot and yields a wrong answer silently. *)

(* --- A mutable cell.  The environment holds the ref itself, so code the
   macro returns and code the module writes by hand see one cell. *)

let counter = ref 0

macro bump () = << incr counter; !counter >>

let c1 = $(bump ())
let () = incr counter
let c2 = $(bump ())
let c3 = !counter

(* --- A shadowed name: the macro captures the binding in scope where the
   macro is written, not the earlier one of the same name. *)

let shadowed = 111
let shadowed = 222

macro which () = << shadowed >>

let s = $(which ())

(* --- A name rebound after the macro.  Capture is of the entity in scope
   at the macro, so a later binding of the same name is a different
   entity and both splices see the earlier one. *)

let rebound = 10

macro before () = << rebound >>

let r_before = $(before ())

let rebound = 20

let r_after = $(before ())
let r_now = rebound

(* --- Several captures of different representations: an immediate, a
   boxed float, a string, a tuple, a function.  A wrong environment index
   would swap them, and the types alone would not catch it. *)

let an_int = 7
let a_float = 2.5
let a_string = "cap"
let a_pair = (3, "p")
let a_fun x = x * 5

macro mixed () =
  << (an_int, a_float, a_string, fst a_pair, snd a_pair, a_fun 4) >>

let (m1, m2, m3, m4, m5, m6) = $(mixed ())

(* --- An exception (an extension constructor): a quoted mention closes
   over the constructor's run-time block, so the handler in ordinary code
   catches what the returned code raises. *)

exception Local_exn of int

macro raiser n = << raise (Local_exn $(Expr.int n)) >>

let e1 = try $(raiser 33) with Local_exn n -> n

(* ... and a mention in a quoted pattern. *)

macro catcher c = << try $c with Local_exn n -> n * 2 >>

let e2 = $(catcher << raise (Local_exn 4) >>)

(* --- A constructor of an ordinary (non-extensible) type needs no
   capture -- its tag is static -- but the value must still arrive. *)

type colour = Red | Green | Blue of int

let describe = function Red -> 1 | Green -> 2 | Blue n -> n

macro coloured () = << describe (Blue 9) + describe Green >>

let k = $(coloured ())

(* --- A module-qualified name.  The root is a local module, so the
   environment captures the module BLOCK and the projection stays in the
   returned code. *)

module Inner = struct
  let a = 5
  let b = 6
  module Deeper = struct let d = 60 end
end

macro qualified () = << Inner.a * 100 + Inner.b >>

let q = $(qualified ())

macro qualified_deep () = << Inner.Deeper.d + Inner.a >>

let qd = $(qualified_deep ())

(* --- A stdlib name: rooted at a persistent identifier, so it is a
   projection valid wherever the unit is linked and is NOT captured. *)

macro from_stdlib () = << List.length [1; 2; 3] + String.length "ab" >>

let l = $(from_stdlib ())

(* --- A macro whose environment mixes a captured local with a global:
   only the local takes a slot, so the indices must skip the global. *)

let local_scale = 4

macro mixed_roots () = << List.length [1; 2] * local_scale >>

let mr = $(mixed_roots ())

(* --- A capture of a member of a recursive value group. *)

let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1)

macro parity () = << (even 10, odd 10) >>

let (p1, p2) = $(parity ())

(* --- The captured binding is read by a splice much later in the module,
   after many intervening items: the environment slot is built where the
   macro sits and must still be the one the later splice reads. *)

let far = 77
macro far_away () = << far + 1 >>
let filler1 = 1
let filler2 = "two"
module Filler3 = struct let x = 3 end
let filler4 = ref 4
let fa = $(far_away ())

let () =
  Printf.printf "ref: %d %d %d\n" c1 c2 c3;
  Printf.printf "shadowed: %d\n" s;
  Printf.printf "rebound: %d %d %d\n" r_before r_after r_now;
  Printf.printf "mixed: %d %g %s %d %s %d\n" m1 m2 m3 m4 m5 m6;
  Printf.printf "exn: %d %d\n" e1 e2;
  Printf.printf "constructor: %d\n" k;
  Printf.printf "qualified: %d %d\n" q qd;
  Printf.printf "stdlib: %d %d\n" l mr;
  Printf.printf "recursive: %b %b\n" p1 p2;
  Printf.printf "far: %d %d %s %d %d\n"
    fa filler1 filler2 Filler3.x !filler4
