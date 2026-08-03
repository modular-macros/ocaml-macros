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

(* A macro whose sole parameter is a TUPLE PATTERN, spliced in the unit
   that DEFINES it.  Under the native driver such a function used to be
   translated with the tupled calling convention -- whose [params] are
   the tuple's flattened components -- while the macro-reference arm
   applies a macro's compile-time function to its environment CURRIED,
   and [Bytegen] (the static program is bytecode whichever driver builds
   it) compiles every function as if curried.  So [add] below became a
   3-ary closure applied to 2 arguments, and the partial application that
   resulted was used as the code the macro returned: the static program
   died of SIGSEGV.  Compiling this file under [ocamlopt] is therefore
   most of the test; the values are checked too, since a convention that
   merely disagreed on the ORDER of the arguments would compile and give
   a wrong answer.

   The native arm is the whole point: with only the bytecode arm nothing
   here would ever have failed. *)

(* --- The bare shape: one tuple parameter, no capture. *)

macro add (a, b) = << $a + $b >>

let t1 = $(add (<< 1 >>, << 2 >>))

(* Wider tuples: the environment parameter is prepended to a parameter
   list of whatever width the flattening produced. *)

macro add3 (a, b, c) = << $a + $b + $c >>
macro add5 (a, b, c, d, e) = << $a + $b + $c + $d + $e >>

let t2 = $(add3 (<< 10 >>, << 20 >>, << 30 >>))
let t3 = $(add5 (<< 1 >>, << 2 >>, << 4 >>, << 8 >>, << 16 >>))

(* A nested tuple pattern: the outer tuple flattens, the inner one is
   destructured in the body. *)

macro nest ((a, b), c) = << ($a * 100) + ($b * 10) + $c >>

let t4 = $(nest ((<< 7 >>, << 5 >>), << 3 >>))

(* --- A tuple parameter beside a captured run-time name.  The macro's
   environment is passed as the FIRST argument, so a mis-shaped
   parameter list would make the body read a component of the tuple in
   place of the environment (or the reverse). *)

let scale = 11
let bias = 4

macro blend (a, b) = << (($a + $b) * scale) + bias >>

let t5 = $(blend (<< 2 >>, << 3 >>))

(* The tuple's components and the capture meeting under a run-time
   binder in the returned code. *)

macro mk_pair_fun (a, b) = << fun x -> (x * $a) + ($b * scale) >>

let t6 = $(mk_pair_fun (<< 6 >>, << 2 >>)) 9

(* --- Ordinary values, not just code, in the tuple. *)

macro repeat (n, c) =
  let rec go i = if i = 0 then << 0 >> else << $c + $(go (i - 1)) >> in
  go n

let t7 = $(repeat (4, << 25 >>))

(* --- A tuple pattern that is not the only parameter: this one stays
   curried even under the native translation, and must keep working. *)

macro mixed (a, b) c = << ($a + $b) * $c >>

let t8 = $(mixed (<< 3 >>, << 4 >>) << 5 >>)

macro leading c (a, b) = << ($a - $b) * $c >>

let t9 = $(leading << 6 >> (<< 9 >>, << 2 >>))

(* --- A [function] with several tuple cases: the flattening walks every
   case, and the group still has to agree with the call. *)

macro classify = function
  | (0, y) -> << 1000 + $y >>
  | (1, y) -> << 2000 + $y >>
  | (_, y) -> << 3000 + $y >>

let t10 = $(classify (0, << 5 >>))
let t11 = $(classify (1, << 7 >>))
let t12 = $(classify (9, << 2 >>))

(* --- A [macro rec] group whose members take tuples: the members share
   one environment, prepended to each member's flattened parameters. *)

let unit_step = 3

macro rec down (n, acc) =
  if n = 0 then acc else down (n - 1, << unit_step + $acc >>)

let t13 = $(down (5, << 0 >>))

(* --- A tuple-parameter macro defined in a nested module and spliced in
   the same unit: the compile-time function still lives in this unit's
   macro block. *)

module M = struct
  let inner = 100
  macro combine (a, b) = << ($a * inner) + $b >>
end

let t14 = $(M.combine (<< 2 >>, << 34 >>))

(* --- A tuple-parameter macro passed to, and returned from, ordinary
   compile-time functions: the reference site is not the call site. *)

macro sub (a, b) = << $a - $b >>

let t15 = $(let apply f = f (<< 30 >>, << 8 >>) in apply sub)

let () =
  Printf.printf "bare: %d\n" t1;
  Printf.printf "wide: %d %d\n" t2 t3;
  Printf.printf "nested: %d\n" t4;
  Printf.printf "capture: %d %d\n" t5 t6;
  Printf.printf "values: %d\n" t7;
  Printf.printf "mixed: %d %d\n" t8 t9;
  Printf.printf "cases: %d %d %d\n" t10 t11 t12;
  Printf.printf "rec: %d\n" t13;
  Printf.printf "module: %d\n" t14;
  Printf.printf "indirect: %d\n" t15
