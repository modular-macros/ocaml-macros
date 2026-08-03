(* TEST
 toplevel;
*)

(* An anonymous template application in the toplevel: evaluated for
   its effects, binding nothing. *)

module F[X : sig val v : int end] = struct
  let () = Printf.printf "init:%d\n" X.v
end;;

module V = struct let v = 7 end;;

module _ = F[V];;

let after = 5;;
Printf.printf "after=%d\n" after;;
