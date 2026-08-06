(* Auxiliary module for macro_env_cross.ml.  Not a test in its own right. *)

let helper x = x + 1

macro gen () = << helper 5 >>

(* A macro whose callee is a macro of this same module.  The callee's
   environment must ride in this one's, so that a caller in another module
   reaches it through this module's block rather than by a local name. *)
macro outer () = << 10 + $(gen ()) >>

(* A macro that quotes nothing.  Its environment is empty. *)
macro closed () = << 99 >>
