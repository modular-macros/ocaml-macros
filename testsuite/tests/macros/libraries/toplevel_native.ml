(* TEST
 toplevel.opt;
*)

(* The native toplevel does not load units for compile-time code. *)

#static_use "counter.cmo";;
