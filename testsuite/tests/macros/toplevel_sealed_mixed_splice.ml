(* TEST
 toplevel;
*)

(* A signature-sealed mixed module read through its coercion by a
   splice in the same phrase: the sealing signature reorders the
   macro and value components and hides a binding, so the coercion is
   nontrivial, and the splice reads the macro through it before the
   phrase completes.  This shape was recorded as untested in
   LIMITATIONS.md when the layout-parity fix landed. *)

module M : sig macro mm : unit -> int expr val v : int end = struct
  let hidden = 99
  macro mm () = << 7 >>
  let v = hidden - 94
end
let x = $(M.mm ()) + M.v;;

Printf.printf "x=%d\n" x;;
