(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A generator is a function of compile-time code.  Unlike a static
   binding, the code that it builds may mention the run-time values in
   whose scope it is defined. *)

let scale = 3

$let[..] scaled x = << $x * scale >>
$let[..] rec power n x =
  if n = 0 then << 1 >> else << $x * $(power (n - 1) x) >>
$let[..] twice f x = f (f x)
$let[..] constant () = << scale >>

let a = $(scaled << 2 >>)
let b = $(power 3 << a >>)
let c = $(twice scaled << 1 >>) + $(constant ())

(* A generator may use static bindings and other generators. *)

$let depth = 2
$let[..] deep x = power depth (scaled x)

let d = $(deep << 1 >>)

(* A generator is a component of its module. *)

module M = struct
  let unit = 1
  $let[..] succ x = << $x + unit >>
end

let e = $(M.succ << 1 >>)

module type S = sig
  val unit : int
  $val[..] succ : int expr -> int expr
end

module N : S = M

(* A signature may hide it. *)

module O : sig val unit : int end = M

(* An opened module serves its generators to compile-time code. *)

open M

let f = $(succ << 2 >>)
