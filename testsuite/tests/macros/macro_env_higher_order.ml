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

(* Higher-order use of macros that capture.  A $gen reference carries its
   environment from the point where the $gen is NAMED, so partial
   application, composition and passing a $gen to another $gen all have
   to keep the environment attached without a second reference site. *)

let base = 3
let factor = 6
let step = 100
let offset = 7

(* --- A $gen taking code arguments and composing them, while also
   capturing.  Both the spliced arguments and the projection from the
   environment must land in the one returned term. *)

$gen add a b = << $a + $b * base >>

let h1 = $(add << 1 >> << 2 >>)

(* --- The returned code contains a FUNCTION that captures: the
   projection sits under a run-time binder, so the closure the program
   builds must read the environment slot, not a stale copy. *)

$gen mk_scaler () = << fun x -> x * factor >>

let scaler = $(mk_scaler ())
let h2 = scaler 7

(* The function's own parameter travels inside the term (it stays a
   variable) while [offset] is captured: the two kinds of name meet in
   one quotation. *)

$gen mk_shifter k = << fun x -> $(k << x >>) >>

let shifter = $(mk_shifter (fun x -> << $x + offset >>))
let h3 = shifter 5

(* --- A $gen applied to the result of another $gen.  [gen]'s code is
   spliced twice, so the environment projection is duplicated; both
   copies must still address the same slot. *)

$gen gen () = << step >>
$gen twice c = << $c + $c >>

let h4 = $(twice (gen ()))

(* --- Currying and partial application at compile time. *)

$gen three a b c = << ($a * 100 + $b * 10 + $c) + base >>

let h5 = $(let f = three << 1 >> in let g = f << 2 >> in g << 3 >>)

(* --- A named $gen passed to a higher-order $gen: the environment is
   fixed at the reference [incr_code], not where [apply] applies it. *)

$gen apply f x = f x
$gen incr_code x = << $x + step >>

let h6 = $(apply incr_code << 41 >>)

(* Two macros with different environments passed to the same
   higher-order $gen: each must arrive with its own. *)

$gen use_both f g = << $(f << 1 >>) * 1000 + $(g << 1 >>) >>

$gen plus_base x = << $x + base >>
$gen plus_offset x = << $x + offset >>

let h7 = $(use_both plus_base plus_offset)

(* --- Quoted binders must NOT be captured: a name bound inside the
   quotation does not exist where the environment is built, so the
   free-variable analysis has to subtract it. *)

let acc = 4

$gen with_local () = << let acc = 1000 in acc + 1 >>

let h8 = $(with_local ())

$gen with_local_and_capture () = << let tmp = 1000 in tmp + acc >>

let h9 = $(with_local_and_capture ())

$gen shadow_param () = << fun acc -> acc + 1 >>

let h10 = $(shadow_param ()) 10

$gen shadow_pattern () = << (function (acc, _) -> acc * 2) (21, 0) >>

let h11 = $(shadow_pattern ())

(* A quoted binder of the same name beside a real capture in the same
   quotation: only the latter takes an environment slot. *)

$gen both_kinds () = << (let acc = 50 in acc) + acc >>

let h12 = $(both_kinds ())

let () =
  Printf.printf "compose: %d\n" h1;
  Printf.printf "closure: %d %d\n" h2 h3;
  Printf.printf "of-$gen: %d\n" h4;
  Printf.printf "curried: %d\n" h5;
  Printf.printf "passed: %d %d\n" h6 h7;
  Printf.printf "binders: %d %d %d %d %d\n" h8 h9 h10 h11 h12
