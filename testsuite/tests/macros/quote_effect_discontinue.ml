(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* discontinue across a quotation scope: vanilla effect semantics
   deliver the exception at the perform point, inside the splice, where
   the try catches it and yields << 7 >>.  The forwarding branch used
   to unwind in the handler frame instead, leaking the suspended
   continuation and reporting Not_found as a compile-time failure. *)

type _ Effect.t += R : unit -> int expr Effect.t

macro m () =
  match << fun x -> $( try Effect.perform (R ()) with Not_found -> << 7 >> ) >> with
  | y -> y
  | effect R (), k -> Effect.Deep.discontinue k Not_found

let f = $(m ())
let () = print_int (f 1); print_newline ()
