(* Auxiliary module for macro_env_nested.ml.  Not a test in its own right. *)

(* A module whose only macros live in nested modules: the emission test for
   the macro object must look inside literal sub-structures, not just at the
   top level. *)

module N = struct
  macro g () = << 7 >>
end

module Outer = struct
  module Inner = struct
    macro h () = << 21 >>
  end
end
