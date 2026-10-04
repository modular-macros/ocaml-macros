(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A generator is called from a splice, and the code that it builds
   takes the place of the splice.  That code may mention the run-time
   values in whose scope the generator is defined, wherever the splice
   stands. *)

let scale = 3
let offset = ref 0

$let[..] scaled x = << $x * scale + !offset >>
$let[..] constant () = << scale >>

let a = $(scaled << 2 >>)
let () = offset := 1
let b = $(scaled << 2 >>) + $(constant ())

(* Recursive generators, and generators that call generators *)

$let[..] rec power n x =
  if n = 0 then << 1 >> else << $x * $(power (n - 1) x) >>

$let[..] rec even n x = if n = 0 then << $x + scale >> else odd (n - 1) x
and odd n x = if n = 0 then << $x - scale >> else even (n - 1) x

$let[..] cube x = power 3 (scaled x)

let c = $(power 5 << 2 >>)
let d = $(even 3 << 10 >>) + $(odd 3 << 10 >>)
let e = $(cube << 1 >>)

(* A generator is a value of compile-time code: it may be passed, and
   chosen. *)

$let[..] twice f x = f (f x)
$let[..] pick flag = if flag then scaled else cube

let f = $(twice scaled << 1 >>)
let g = $((pick true) << 1 >>) + $((pick false) << 1 >>)

(* Generators and static bindings *)

$let depth = 2
$let[..] deep x = power depth x

let h = $(deep << 7 >>)

(* A generator in a module, and one hidden by a signature that another
   generator calls *)

module M = struct
  let unit = 1
  $let[..] succ x = << $x + unit >>
  let two = $(succ << unit >>)
end

module N : sig
  $val[..] next : int expr -> int expr
end = struct
  let step = 10
  $let[..] hidden x = << $x + step >>
  $let[..] next x = hidden (hidden x)
end

module A = N

let i = $(M.succ << M.two >>)
let j = $(N.next << 0 >>) + $(A.next << 1 >>)

(* The code is built where the splice stands, and may mention the
   variables bound there. *)

let k y = $(scaled << y >>)

(* A generator may be passed to a static binding as a value and called
   there. *)

$let apply f = f ()
let l = $(apply (fun () -> scaled << 2 >>))

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d %d %d %d\n"
    a b c d e f g h i j (k 5) l
