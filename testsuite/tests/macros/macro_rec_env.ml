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

(* Mutually recursive macros that call each other *and* quote run-time
   bindings: the group shares one environment -- the union of the members'
   captured roots -- and a sibling call passes it straight through, so each
   member's body projects from a layout every member agrees on. *)

let helper x = x * 3
let dec x = x - 1

macro rec a n = if n = 0 then << helper 1 >> else << dec $(b (n-1)) >>
and b n = if n = 0 then << helper 2 >> else << helper $(a (n-1)) >>

let v = $(a 3)
let w = $(b 2)

(* An all-empty group: no member quotes a run-time binding, so the shared
   environment is empty and the members' slots stay immediates. *)
macro rec ping n = if n = 0 then << 0 >> else << 1 + $(pong (n-1)) >>
and pong n = if n = 0 then << 10 >> else << 2 + $(ping (n-1)) >>

let p = $(ping 3)

let () = Printf.printf "v = %d\nw = %d\np = %d\n" v w p
