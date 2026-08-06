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

(* A macro of dynamically-selected identity -- chosen by compile-time
   control flow, returned from a macro, stored and retrieved.
   The environment design set this as its boundary, on the grounds that
   there is no single reference site at which to attach one right
   environment, and left it to verify that such a program is
   either unrepresentable or rejected, "never silently emitting code
   that names an out-of-scope environment".

   It is neither: it works, and the reason is the mechanism itself.  The
   environment is attached where the macro is NAMED -- at [g1] and [g2]
   below, each becoming a partial application already carrying its own
   environment -- so by the time compile-time control flow chooses
   between them there is nothing left for the choice to get wrong.  The
   values here are what pins that: [g1] and [g2] capture DIFFERENT
   bindings, so a selection that lost or crossed an environment would
   print the other one's answer rather than fail. *)

let helper x = x + 1
let other x = x * 100
let extra = 7

macro g1 () = << helper 5 >>
macro g2 () = << other 5 >>

(* Chosen by an [if] in a macro body, and applied at the splice. *)

macro pick b = if b then g1 else g2

let a1 = $((pick true) ())
let a2 = $((pick false) ())

(* Chosen by a match, and applied inside the macro that chose. *)

macro apply_chosen n =
  let m = match n with 0 -> g1 | _ -> g2 in
  << 1000 + $(m ()) >>

let b1 = $(apply_chosen 0)
let b2 = $(apply_chosen 1)

(* Stored in a data structure and retrieved: no reference to the macro
   survives to the application site at all. *)

macro nth n = List.nth [ g1; g2 ] n

let c1 = $((nth 0) ())
let c2 = $((nth 1) ())

(* Stored in a mutable cell at compile time and read back -- "stored and
   retrieved", the third of the three shapes the design considered. *)

macro from_cell b =
  let r = ref g1 in
  if b then r := g2;
  !r

let c3 = $((from_cell false) ())
let c4 = $((from_cell true) ())

(* Mixed with an anonymous compile-time function that captures a binding
   of its own: the chosen thing is sometimes a macro carrying an
   environment and sometimes a closure that quotes directly. *)

macro pick_mixed b = if b then g1 else (fun () -> << other 5 + extra >>)

let d1 = $((pick_mixed true) ())
let d2 = $((pick_mixed false) ())

(* Selected once and applied twice, so one environment serves two
   holes. *)

macro twice_over b =
  let m = if b then g1 else g2 in
  << $(m ()) + $(m ()) >>

let e1 = $(twice_over true)
let e2 = $(twice_over false)

let () =
  Printf.printf "if: %d %d\n" a1 a2;
  Printf.printf "match: %d %d\n" b1 b2;
  Printf.printf "list: %d %d\n" c1 c2;
  Printf.printf "cell: %d %d\n" c3 c4;
  Printf.printf "mixed: %d %d\n" d1 d2;
  Printf.printf "reused: %d %d\n" e1 e2
