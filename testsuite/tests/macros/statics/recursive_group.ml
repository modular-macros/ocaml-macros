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

(* The quotation may be built by a generator of the body, and the
   reference may be a call. *)

module rec C : sig val f : unit -> int val r : unit -> int end = struct
  $let[..] twice () = << fun () -> D.g () * 2 >>
  let f () = $(<< D.g () >>)
  let r = $(twice ())
end
and D : sig val g : unit -> int end = struct
  let g () = 7
end
let () = Printf.printf "%d %d\n" (C.f ()) (C.r ())
