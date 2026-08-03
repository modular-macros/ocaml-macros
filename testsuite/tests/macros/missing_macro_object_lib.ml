(* The library whose compile-time object missing_macro_object.ml deletes
   before compiling.  This file is compiled, but only for the objects it
   leaves behind. *)

macro g () = << 7 >>
