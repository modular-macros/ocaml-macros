(* TEST
 readonly_files = "splices_exit_numbers_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "splices_exit_numbers_lib.ml splices_exit_numbers.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "splices_exit_numbers_lib.ml splices_exit_numbers.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The code of a splice keeps the numbers of the static exceptions that
   the compilation of its unit drew, which the numbers that this
   compilation draws later must not meet.  The native back end numbers
   the loop of each function after this unit's translation, and the
   functions are enough for those numbers to pass the handlers' inside
   the loops; a loop numbered as a handler inside it stops the
   compilation, or makes it run for ever. *)

let unescape1 = $(Splices_exit_numbers_lib.unescape ())
let unescape2 = $(Splices_exit_numbers_lib.unescape ())
let unescape3 = $(Splices_exit_numbers_lib.unescape ())
let unescape4 = $(Splices_exit_numbers_lib.unescape ())
let unescape5 = $(Splices_exit_numbers_lib.unescape ())
let unescape6 = $(Splices_exit_numbers_lib.unescape ())
let unescape7 = $(Splices_exit_numbers_lib.unescape ())
let unescape8 = $(Splices_exit_numbers_lib.unescape ())

let () =
  List.iter (fun f -> print_string (f "\"a\\nb\""))
    [unescape1; unescape2; unescape3; unescape4;
     unescape5; unescape6; unescape7; unescape8];
  print_newline ()
