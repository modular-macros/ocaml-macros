(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The functor or the argument of an application may be an application
   itself: it is bound as a hidden application item of its own, around
   the item. *)

module G$(X : sig val v : int end) = struct
  let w = X.v + 1
  let () = Printf.printf "[G %d]" w
  $let[..] g () = << w >>
end
module F$(Y : sig val w : int $val[..] g : unit -> int expr end) = struct
  let z = Y.w * 2 + $(Y.g ())
end
module C$(X : sig val a : int end)$(Y : sig val b : int end) = struct
  let v = X.a * 10 + Y.b
  $let[..] k () = << v >>
end

(* A curried template applied twice in one item. *)
module M1 = C$(struct let a = 4 end)$(struct let b = 5 end)
module A = struct let a = 1 end
module B = struct let b = 2 end
module M2 = C$(A)$(B)
let () = Printf.printf "%d %d %d\n" M1.v M2.v $(M2.k ())

(* An application as the argument, to a literal and to a path. *)
module M3 = F$(G$(struct let v = 1 end))
module V = struct let v = 10 end
module M4 = F$(G$(V))
let () = Printf.printf "%d %d\n" M3.z M4.z

(* Under a constraint; anonymous; included; a literal template. *)
module M5 = F$((G$(struct let v = 2 end) : sig val w : int $val[..] g : unit -> int expr end))
module _ = F$(G$(struct let v = 3 end))
include F$(G$(struct let v = 4 end))
module M6 =
  (functor $(X : sig val a : int end) $(Y : sig val b : int end) ->
     struct let v = X.a + Y.b end)$(struct let a = 1 end)$(struct let b = 2 end)
let () = Printf.printf "%d %d %d\n" M5.z z M6.v

(* Inside a functor body, applied twice; inside a template body; and
   inside a functor body inside a template body. *)
module H (P : sig val v : int end) = struct module M = F$(G$(P)) end
module H1 = H (struct let v = 1 end)
module H2 = H (struct let v = 2 end)
module T$(P : sig val v : int end) = struct
  module M = F$(G$(P))
  module N = F$(G$(struct let v = P.v * 10 end))
  module K (Q : sig end) = struct module M = F$(G$(P)) end
  module R = K (struct end)
end
module T1 = T$(struct let v = 1 end)
let () = Printf.printf "%d %d %d %d %d\n" H1.M.z H2.M.z T1.M.z T1.N.z T1.R.M.z

(* The innermost argument carries a static. *)
module S$(X : sig $val s : int end) = struct
  $let t = X.s + 1
  let v = $(Expr.int t)
end
module R$(Y : sig val v : int end) = struct let w = Y.v * 2 end
module M7 = R$(S$(struct $let s = 20 end))
let () = Printf.printf "%d\n" M7.w
