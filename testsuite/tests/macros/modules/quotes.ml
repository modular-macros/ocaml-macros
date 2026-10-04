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

(* Quoted code is run-time code: a package built in it is built from
   the run-time components of its module, and a module bound in it is
   a run-time module. *)

module type S = sig val v : int end

module M = struct
  let v = 7
  $let[..] two () = << 2 >>
end

$let[..] packed () = << (module M : S) >>
let a = let module P = (val $(packed ()) : S) in P.v

$let[..] bound () = << let module N = M in N.v + $(N.two ()) >>
let b = $(bound ())

$let[..] alias_of_static () = << let module N = M in N.v >>
let c = $(alias_of_static ())

let () = Printf.printf "%d %d %d\n" a b c
