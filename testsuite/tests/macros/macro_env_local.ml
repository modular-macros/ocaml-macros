(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* A macro may quote a run-time binding of its own module.  The binding is
   not available at compile time -- the compile-time translation drops
   run-time bindings -- so the macro is closure-converted over it: it takes
   an environment carrying the names it quotes, and the quoted code projects
   out of that environment rather than naming the binding directly. *)

let helper x = x + 1

macro gen () = << helper 5 >>

let z = $(gen ())

(* A macro calling another macro of the same module has to pass the callee's
   environment, which it cannot name directly: inside this module it is a
   local binding, outside it is a field of this module.  So the callee's
   environment is itself a free variable of the caller, and rides in the
   caller's environment. *)

macro outer () = << 10 + $(gen ()) >>

let w = $(outer ())

(* Quoting several bindings, and one that is only reached through a macro. *)

let twice x = x * 2

macro both () = << $(gen ()) + twice 3 >>

let v = $(both ())

let () = Printf.printf "z = %d\nw = %d\nv = %d\n" z w v
