(* TEST
 flags = "-g";
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

(* Generators under -g: a recursive generator, and generators with
   environments, carry no debug events in their compile-time functions. *)
$let[..] rec pow x n =
  if n = 0 then << 1 >>
  else << $x * $(pow x (n - 1)) >>
let pow5 x = $(pow << x >> 5)

let helper x = x + 1
$let[..] gen () = << helper 5 >>
let z = $(gen ())
$let[..] outer () = << 10 + $(gen ()) >>
let w = $(outer ())
let twice x = x * 2
$let[..] both () = << $(gen ()) + twice 3 >>
let v = $(both ())
let () = Printf.printf "%d %d %d %d\n" (pow5 2) z w v
