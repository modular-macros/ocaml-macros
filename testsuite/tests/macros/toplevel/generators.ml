(* TEST
 toplevel;
*)

(* A generator of a phrase is called from the splices of later phrases
   and of its own.  The code that it builds means the values in scope
   at its definition, whatever is bound later. *)

let scale = 3;;
$let[..] scaled x = << $x * scale >>;;
let a = $(scaled << 2 >>);;
let scale = 100;;
let b = $(scaled << 2 >>);;
$let[..] twice x = scaled (scaled x)  let c = $(twice << 1 >>);;

(* Rebinding a generator does not change the generators that call
   it. *)

$let[..] scaled x = << $x + 1 >>;;
let d = $(twice << 1 >>) + $(scaled << 1 >>);;

(* Recursive generators; generators as values *)

$let[..] rec power n x =
  if n = 0 then << 1 >> else << $x * $(power (n - 1) x) >>;;
let e = $(power 3 << 2 >>);;
$let[..] pick flag = if flag then scaled else twice;;
let f = $((pick true) << 1 >>) + $((pick false) << 1 >>);;

(* A generator is not a run-time value. *)

let g = scaled;;

(* Generators in modules, through an alias, an ascription and an
   include *)

module M = struct
  let unit = 1
  $let[..] succ x = << $x + unit >>
  let two = $(succ << unit >>)
end;;
let h = $(M.succ << M.two >>);;
module A = M;;
module B = (M : sig $val[..] succ : int expr -> int expr end);;
module C = struct include M end;;
include M;;
let i =
  $(A.succ << 0 >>) + $(B.succ << 0 >>) + $(C.succ << 0 >>) + $(succ << 0 >>);;

(* In a recursive module *)

module rec R : sig
  $val[..] r : unit -> int expr
  val v : int
end = struct
  $let[..] r () = << 42 >>
  let v = $(r ())
end;;
R.v + $(R.r ());;
