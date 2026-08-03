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

(* Higher-order use of macros that capture.  A macro reference carries its
   environment from the point where the macro is NAMED, so partial
   application, composition and passing a macro to another macro all have
   to keep the environment attached without a second reference site. *)

let base = 3
let factor = 6
let step = 100
let offset = 7

(* --- A macro taking code arguments and composing them, while also
   capturing.  Both the spliced arguments and the projection from the
   environment must land in the one returned term. *)

macro add a b = << $a + $b * base >>

let h1 = $(add << 1 >> << 2 >>)

(* --- The returned code contains a FUNCTION that captures: the
   projection sits under a run-time binder, so the closure the program
   builds must read the environment slot, not a stale copy. *)

macro mk_scaler () = << fun x -> x * factor >>

let scaler = $(mk_scaler ())
let h2 = scaler 7

(* The function's own parameter travels inside the term (it stays a
   variable) while [offset] is captured: the two kinds of name meet in
   one quotation. *)

macro mk_shifter k = << fun x -> $(k << x >>) >>

let shifter = $(mk_shifter (fun x -> << $x + offset >>))
let h3 = shifter 5

(* --- A macro applied to the result of another macro.  [gen]'s code is
   spliced twice, so the environment projection is duplicated; both
   copies must still address the same slot. *)

macro gen () = << step >>
macro twice c = << $c + $c >>

let h4 = $(twice (gen ()))

(* --- Currying and partial application at compile time. *)

macro three a b c = << ($a * 100 + $b * 10 + $c) + base >>

let h5 = $(let f = three << 1 >> in let g = f << 2 >> in g << 3 >>)

(* --- A named macro passed to a higher-order macro: the environment is
   fixed at the reference [incr_code], not where [apply] applies it. *)

macro apply f x = f x
macro incr_code x = << $x + step >>

let h6 = $(apply incr_code << 41 >>)

(* Two macros with different environments passed to the same
   higher-order macro: each must arrive with its own. *)

macro use_both f g = << $(f << 1 >>) * 1000 + $(g << 1 >>) >>

macro plus_base x = << $x + base >>
macro plus_offset x = << $x + offset >>

let h7 = $(use_both plus_base plus_offset)

(* --- Quoted binders must NOT be captured: a name bound inside the
   quotation does not exist where the environment is built, so the
   free-variable analysis has to subtract it. *)

let acc = 4

macro with_local () = << let acc = 1000 in acc + 1 >>

let h8 = $(with_local ())

macro with_local_and_capture () = << let tmp = 1000 in tmp + acc >>

let h9 = $(with_local_and_capture ())

macro shadow_param () = << fun acc -> acc + 1 >>

let h10 = $(shadow_param ()) 10

macro shadow_pattern () = << (function (acc, _) -> acc * 2) (21, 0) >>

let h11 = $(shadow_pattern ())

(* A quoted binder of the same name beside a real capture in the same
   quotation: only the latter takes an environment slot. *)

macro both_kinds () = << (let acc = 50 in acc) + acc >>

let h12 = $(both_kinds ())

let () =
  Printf.printf "compose: %d\n" h1;
  Printf.printf "closure: %d %d\n" h2 h3;
  Printf.printf "of-macro: %d\n" h4;
  Printf.printf "curried: %d\n" h5;
  Printf.printf "passed: %d %d\n" h6 h7;
  Printf.printf "binders: %d %d %d %d %d\n" h8 h9 h10 h11 h12
