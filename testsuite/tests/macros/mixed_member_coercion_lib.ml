(* A macro-bearing unit whose RUN components a signature will narrow:
   a submodule, a three-deep nesting, and a plain functor, beside the
   macro that makes the unit's compile-time block real. *)
module Sub = struct let x = 1 let y = 2 end
module Outer = struct
  module Mid = struct module Deep = struct let p = 7 let q = 8 end let r = 9 end
end
module F (Y : sig val v : int end) = struct let g = Y.v let h = 2 end
macro m () = << 10 >>
