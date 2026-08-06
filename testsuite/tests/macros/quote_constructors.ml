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

(* Sweep the lambda constructors through both quote generators, which
   have independent hand-written cases (see the layout note at [type
   lambda] in lambda.mli, and O3.md): section A through the expression
   quoter (quote_lambda), section B through the module-term rebuilder
   (quote_module_lambda).  Values are chosen so a case that drifts --
   swapped fields, a lost arm, a dropped default -- changes the output.
   Objects and classes are excluded from section A: they are rejected in
   staged code (quote_objects_rejected.ml) but work in run-time code,
   which section B covers. *)

(* -- Section A: constructs inside quotations -- *)

macro q_for () = <<
  let s = ref 0 in
  for i = 1 to 4 do s := !s + i done;
  !s >>

macro q_while () = <<
  let s = ref 0 in
  while !s < 7 do s := !s + 3 done;
  !s >>

macro q_strmatch s = <<
  match $s with "alpha" -> 1 | "beta" -> 2 | "gamma" -> 3 | _ -> 4 >>

macro q_try () = <<
  (try List.hd [] with Failure _ -> 41) + 1 >>

macro q_exnmatch () = <<
  match List.nth [ 1; 2 ] 5 with
  | n -> n
  | exception (Failure _ | Invalid_argument _) -> 51 >>

macro q_variants n = <<
  match $n with 0 -> 10 | 1 -> 20 | 2 -> 30 | 3 -> 40 | _ -> 50 >>

macro q_letrec () = <<
  let rec even n = if n = 0 then true else odd (n - 1)
  and odd n = if n = 0 then false else even (n - 1) in
  if even 10 then 61 else 62 >>

(* An internal splice inside a quoted match arm: its build must run
   exactly once (the arm builds used to be inlined twice, once for the
   code and once for the free-variable set). *)
macro one () = << 71 >>
macro q_nested_splice () = <<
  match 1 + 1 with 0 -> 0 | 1 -> 1 | 2 -> $(one ()) | _ -> 9 >>

let a1 = $(q_for ())
let a2 = $(q_while ())
let a3 = $(q_strmatch << "beta" >>)
let a3' = $(q_strmatch << "delta" >>)
let a4 = $(q_try ())
let a5 = $(q_exnmatch ())
let a6 = $(q_variants << 2 >>)
let a6' = $(q_variants << 7 >>)
let a7 = $(q_letrec ())
let a8 = $(q_nested_splice ())

let () =
  Printf.printf "A: %d %d %d %d %d %d %d %d %d %d\n"
    a1 a2 a3 a3' a4 a5 a6 a6' a7 a8

(* -- Section B: constructs enclosing top-level splices -- *)

let acc = ref 0
let () = for i = 0 to $( << 2 >> ) do acc := !acc + i done
let () = while !acc < $( << 10 >> ) do acc := !acc + 1 done
let b1 = !acc
let b2 = (match "k" ^ "ey" with
          | "key" -> $( << 7 >> ) | "other" -> 1 | "more" -> 2 | _ -> 0)
let b3 = try if !acc > 100 then failwith "x" else $( << 8 >> )
         with Failure _ -> 9
let b4 = (match List.nth [ 1 ] 3 with
          | n -> n
          | exception _ -> $( << 12 >> ))
let b5 = (object method m = $( << 11 >> ) end)#m
let b6 = (match !acc mod 4 with
          | 0 -> $( << 100 >> ) | 1 -> 101 | 2 -> $( << 102 >> ) | _ -> 103)
let rec fib n = if n < 2 then $( << 1 >> ) else fib (n - 1) + fib (n - 2)
let b7 = fib 6

let () =
  Printf.printf "B: %d %d %d %d %d %d %d\n" b1 b2 b3 b4 b5 b6 b7
