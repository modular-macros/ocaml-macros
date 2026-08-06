(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* Boxed-number literals in quoted code -- directly quoted, and merely
   present in a splice-free subtree embedded as one constant -- used to
   crash the compiler with an unlocated Failure "unsupported tag 255". *)

let a = $( << 3l >> )
let b = $( << 4L >> )
let c = $( << 5n >> )
let d = $( << Int32.add 1l 2l >> )
let embedded = 100l   (* in a unit with splices, reached via const *)
let () =
  Printf.printf "%ld %Ld %nd %ld %ld\n" a b c d embedded
