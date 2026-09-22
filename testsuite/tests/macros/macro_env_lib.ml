(* Auxiliary module for macro_env_cross.ml.  Not a test in its own right. *)

let helper x = x + 1

$gen gen () = << helper 5 >>

(* A $gen whose callee is a $gen of this same module.  The callee's
   environment must ride in this one's, so that a caller in another module
   reaches it through this module's block rather than by a local name. *)
$gen outer () = << 10 + $(gen ()) >>

(* A $gen that quotes nothing.  Its environment is empty. *)
$gen closed () = << 99 >>
