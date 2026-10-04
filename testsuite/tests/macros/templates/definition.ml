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

(* Definitions without applications: a definition's slot takes its
   position in the run-time block and its component function the same
   position in the compile-time block, and the module around it is
   undisturbed. *)

let before = 1
let base = 10
$let[..] g c = << $c + base >>

module type S0 = sig val v : int $val[..] m : int expr -> int expr end

(* A body with every supported shape: level 0 code over X and the
   enclosing module; a body splice calling an enclosing $let[..] and
   quoting X; a body $let[..] capturing a body binding; a body splice
   calling the argument's $let[..] and quoting a body binding. *)
module F$(X : S0) = struct
  let a = X.v + base
  let b = $( g << X.v * 2 >> )
  $let[..] mm c = << a + $c >>
  let c = $( X.m << a >> )
end

(* An empty environment stays an immediate; a unit parameter is fine. *)
module G$() = struct let u = 1 end

let after = 2

let () = Printf.printf "%d %d %d\n" before base after
