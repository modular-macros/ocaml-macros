(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A splice in the body of a recursive module may quote another module
   of the group.  The group is evaluated in the order the quoted
   references need, as it is for direct references. *)

module rec A : sig val p : int end = struct
  let p = $(<< B.q >>)
end
and B : sig val q : int end = struct
  let q = 5
end
let () = Printf.printf "%d\n" A.p
