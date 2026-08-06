(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* A top-level splice on the right of [let rec] used to abort the
   compiler ("letrec: No size found for Static binding"): the typing
   layer classified splices as statically sized while the letrec
   compiler gave the hole no size.  A self-reference-free binding now
   compiles as an ordinary let. *)

macro m () = << (fun () -> 42) >>
let rec x = $(m ())
let () = print_int (x ()); print_newline ()
