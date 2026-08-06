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

(* Template functor DEFINITIONS (3B-SCHEME step 3): a definition
   compiles -- its environment slot at its position in the run-time
   block, F_fn at the same position in the macro block -- and the module
   around it is undisturbed.  Applications are the next step, so nothing
   here instantiates F; the layout across F's slot and the macro-block
   route are exercised cross-module in the standalone repository
   (tests/bytecode/template_def).  The toplevel still rejects
   definitions (template_functors.ml pins that). *)

let before = 1
let base = 10
macro g c = << $c + base >>

module type S0 = sig val v : int macro m : int expr -> int expr end

(* A body with every supported shape: level 0 code over X and the
   enclosing module; a body splice calling an enclosing macro and
   quoting X; a body macro capturing a body binding; a body splice
   calling the argument's macro and quoting a body binding. *)
module F[X : S0] = struct
  let a = X.v + base
  let b = $( g << X.v * 2 >> )
  macro mm c = << a + $c >>
  let c = $( X.m << a >> )
end

(* An empty environment stays an immediate; a unit parameter is fine. *)
module G[] = struct let u = 1 end

let after = 2

let () = Printf.printf "%d %d %d\n" before base after
