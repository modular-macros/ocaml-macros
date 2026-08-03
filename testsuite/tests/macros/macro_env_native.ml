(* TEST
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Native (store-form) counterpart of macro_env_local: a macro quoting a
   run-time binding of its own module, used by a top-level splice in that
   same module.  The store form does not keep the environment slot in
   scope at the hole; the baked access names the module block's field
   instead (DECISIONS.md D-auto-14). *)

let helper x = x + 1

macro gen () = << helper 5 >>

let z = $(gen ())

(* Chaining within the module. *)

macro outer () = << 10 + $(gen ()) >>

let w = $(outer ())

(* A macro of a nested module: the baked access reaches the slot through
   the enclosing module's block. *)

module N = struct
  let h x = x * 2
  macro g () = << h 21 >>
end

let n = $(N.g ())

let () = Printf.printf "z = %d\nw = %d\nn = %d\n" z w n
