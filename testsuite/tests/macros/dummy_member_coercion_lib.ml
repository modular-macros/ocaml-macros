(* Auxiliary module for dummy_member_coercion.ml and
   toplevel_twin_dummy_coercion.ml.  Not a test in its own right.  No
   macros: at the compile-time stage of an includer, every component
   below is a dummy. *)

module Sub = struct
  let x = 1
  let y = 2
end

let v = 42
