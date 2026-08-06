(* Auxiliary module for macro_rec_env_cross.ml.  Not a test in its own
   right. *)

let helper x = x * 3
let dec x = x - 1

macro rec a n = if n = 0 then << helper 1 >> else << dec $(b (n-1)) >>
and b n = if n = 0 then << helper 2 >> else << helper $(a (n-1)) >>
