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

(* Splices quoting variables whose binders travel INSIDE the run-time
   term -- function parameters, pattern-bound and shadowed locals -- and,
   by contrast, a module-level binding, whose binder does not: the former
   stay variables through rebuild, the latter is closed over (and, under
   the closure backend's store form, baked as the module block's field).
   The close binds fresh variables carrying the original identifiers, so
   the compile-time part's own bindings of those identifiers -- a
   dummy slot, a macro's function -- are never rebound. *)

let f1 x = x
let f2 x = $( << x >> )
let f3 x y = $( << x + y >> )
let f4 = function (a, b) -> $( << a * b >> )
let f5 x = let x = x + 1 in $( << x >> )

let helper n = n + 1
let z = $( << helper 1 >> )
let f6 x = $( << x * z >> )

(* Or-pattern-bound variables: no address in the environment, and --
   when the or-pattern sits under another pattern -- Matching splits the
   row and renames the binder in the copy.  The hole applies its thunk
   to the variables the splice quotes, so the renaming reaches the
   close.  A macro body quoting one exercises the same path through the
   captured-roots analysis. *)
macro m c = << $c + 1 >>
let f7 = function `A x | `B x -> $( m << x >> )
let f8 = function (`C y | `D y), w -> $( m << y >> ) * w

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d %d\n"
    (f1 1) (f2 2) (f3 3 4) (f4 (5, 6)) (f5 9) z (f6 21)
    (f7 (`A 41)) (f7 (`B 1)) (f8 (`D 10, 3))
