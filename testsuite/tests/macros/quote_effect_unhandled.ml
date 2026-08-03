(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A user effect performed inside a quotation scope with no handler at
   all: the error must name the user's effect, not the quote runtime's
   internal Mute effect, whose own perform used to escape first. *)

type _ Effect.t += U : unit Effect.t

macro m () = << fun x -> $( Effect.perform U; << 1 >> ) >>

let f = $(m ())
let () = print_int (f 0); print_newline ()
