(* TEST
 toplevel;
*)

(* A splice under an expression-level module -- [let module], or a
   first-class module -- used to kill the toplevel session with
   "unknown splice index": the phrase's static pass skipped the nested
   structure (assuming the compile-time pass translated it in its own
   right, which it does not at level 0), so the slot the run pass reads
   was never filled.  Batch always compiled these. *)

macro m () = << 3 >>;;

let q = let module M = struct let y = $(m ()) end in M.y;;

let p = (let module N = struct let z = $(m ()) + 1 end in N.z);;

Printf.printf "q=%d p=%d\n" q p;;
