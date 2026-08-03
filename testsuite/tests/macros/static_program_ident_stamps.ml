(* TEST
 readonly_files = "static_program_ident_stamps_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "static_program_ident_stamps_lib.ml \
     static_program_ident_stamps.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "static_program_ident_stamps_lib.ml \
     static_program_ident_stamps.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Identifier stamps across the static-program boundary
   (the static-program ident-capture bug; its trail is in git history).

   A static program compiles a term built by the process that spawned it:
   [Translquote.quote_module_lambda] rebuilds the run-time part with its
   binders' identities intact, so every stamp in it came from THAT
   process's [Ident.currentstamp], while this one's starts at zero.
   Nothing then stops a pass of the static program from minting a stamp
   the term already uses -- and [Ident.same] compares stamps alone, so a
   fresh binder that lands on a live one CAPTURES it.  Unlike the
   static-exception collision of the native-static-divergence bug,
   which hung the compiler, this one just emits wrong code.

   The shape below is the smallest thing that makes the capture certain
   rather than lucky.  [Closure] inlines [g] at every [h_i], renaming
   [g]'s binders as it goes; [h_i]'s own parameter [x] is substituted
   into the inlined body and stays there, free, inside the scope of all
   forty renamed [x]s.  So [h_i] is miscompiled as soon as ANY of those
   forty fresh stamps equals its parameter's.  Each step past one [h_i]
   costs the parent three stamps and the static program forty, so the
   two numberings close on each other by thirty-seven per step while the
   window that catches them is forty wide: the window cannot be stepped
   over, and thirty steps is far more than the gap needs.  That is why
   the test does not depend on the exact counter arithmetic, only on the
   static program starting its counter lower than the parent left off.

   Natively only: the bytecode emitter mints almost no identifiers, so
   its counter never climbs into the term's range.  The byte arm is here
   to show the same source compiling correctly through the other
   pipeline, and to catch the day [Bytegen] or [Simplif] starts minting
   in earnest.

   The template application is what routes the unit through a static
   program at all; its result carries nothing compile-time, so the unit
   emits no macro object. *)

(* Forty nested binders, all named [x], each used twice by the next
   so that no simplification removes it, and [p] read once at the
   end -- inside all forty scopes, which is where the capture
   lands.  [@inline] because forty bindings are past the size
   [Closure] will inline unasked. *)
let[@inline] g p =
  let x = p + 1 in
  let x = (x lsr 1) + (x land 7) + 2 in
  let x = (x lsr 1) + (x land 7) + 3 in
  let x = (x lsr 1) + (x land 7) + 4 in
  let x = (x lsr 1) + (x land 7) + 5 in
  let x = (x lsr 1) + (x land 7) + 6 in
  let x = (x lsr 1) + (x land 7) + 7 in
  let x = (x lsr 1) + (x land 7) + 8 in
  let x = (x lsr 1) + (x land 7) + 9 in
  let x = (x lsr 1) + (x land 7) + 10 in
  let x = (x lsr 1) + (x land 7) + 11 in
  let x = (x lsr 1) + (x land 7) + 12 in
  let x = (x lsr 1) + (x land 7) + 13 in
  let x = (x lsr 1) + (x land 7) + 14 in
  let x = (x lsr 1) + (x land 7) + 15 in
  let x = (x lsr 1) + (x land 7) + 16 in
  let x = (x lsr 1) + (x land 7) + 17 in
  let x = (x lsr 1) + (x land 7) + 18 in
  let x = (x lsr 1) + (x land 7) + 19 in
  let x = (x lsr 1) + (x land 7) + 20 in
  let x = (x lsr 1) + (x land 7) + 21 in
  let x = (x lsr 1) + (x land 7) + 22 in
  let x = (x lsr 1) + (x land 7) + 23 in
  let x = (x lsr 1) + (x land 7) + 24 in
  let x = (x lsr 1) + (x land 7) + 25 in
  let x = (x lsr 1) + (x land 7) + 26 in
  let x = (x lsr 1) + (x land 7) + 27 in
  let x = (x lsr 1) + (x land 7) + 28 in
  let x = (x lsr 1) + (x land 7) + 29 in
  let x = (x lsr 1) + (x land 7) + 30 in
  let x = (x lsr 1) + (x land 7) + 31 in
  let x = (x lsr 1) + (x land 7) + 32 in
  let x = (x lsr 1) + (x land 7) + 33 in
  let x = (x lsr 1) + (x land 7) + 34 in
  let x = (x lsr 1) + (x land 7) + 35 in
  let x = (x lsr 1) + (x land 7) + 36 in
  let x = (x lsr 1) + (x land 7) + 37 in
  let x = (x lsr 1) + (x land 7) + 38 in
  let x = (x lsr 1) + (x land 7) + 39 in
  let x = (x lsr 1) + (x land 7) + 40 in
  x + p

(* Thirty call sites, each passing its own [x] -- a variable, so that
   [Closure] substitutes it into the body rather than binding it to a
   fresh name of its own. *)
let h1 x = g x
let h2 x = g x
let h3 x = g x
let h4 x = g x
let h5 x = g x
let h6 x = g x
let h7 x = g x
let h8 x = g x
let h9 x = g x
let h10 x = g x
let h11 x = g x
let h12 x = g x
let h13 x = g x
let h14 x = g x
let h15 x = g x
let h16 x = g x
let h17 x = g x
let h18 x = g x
let h19 x = g x
let h20 x = g x
let h21 x = g x
let h22 x = g x
let h23 x = g x
let h24 x = g x
let h25 x = g x
let h26 x = g x
let h27 x = g x
let h28 x = g x
let h29 x = g x
let h30 x = g x

let total =
  h1 1 + h2 2 + h3 3 + h4 4 + h5 5 + h6 6 + h7 7 + h8 8 + h9 9 + h10 10
  + h11 11 + h12 12 + h13 13 + h14 14 + h15 15 + h16 16 + h17 17
  + h18 18 + h19 19 + h20 20 + h21 21 + h22 22 + h23 23 + h24 24
  + h25 25 + h26 26 + h27 27 + h28 28 + h29 29 + h30 30

module V = struct let n = 21 end
module G = Static_program_ident_stamps_lib.F[V]

let () = Printf.printf "%d %d\n" total G.v
