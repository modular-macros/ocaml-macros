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

(* The functor of an application inside a functor body may be bound in
   the body: a partial application, a template defined there, an
   alias.  The hoisted calls are in the scope of the functor's
   compile-time walk. *)

module F$(X : sig val a : int end)$(Y : sig val b : int end) = struct
  let v = X.a * 10 + Y.b
  $let[..] g () = << v >>
end

module G (P : sig val a : int end) = struct
  module H = F$(P)
  module M = H$(struct let b = 7 end)
  module K$(Y : sig val b : int end) = struct
    let v = P.a * 100 + Y.b
    $let[..] g () = << v >>
  end
  module N = K$(struct let b = 8 end)
  module A = F
  module O = A$(P)$(struct let b = 9 end)
  let z = $(M.g ()) + $(N.g ()) + O.v
end
module G1 = G (struct let a = 1 end)
module G2 = G (struct let a = 2 end)
let () = Printf.printf "%d %d %d %d %d\n" G1.M.v G2.M.v G1.N.v G2.N.v G1.z

(* Sealed. *)
module S (P : sig val a : int end) : sig val z : int end = struct
  module H = F$(P)
  module M = H$(struct let b = 1 end)
  let z = M.v
end
module S1 = S (struct let a = 3 end)
let () = Printf.printf "%d\n" S1.z

(* Under a template body. *)
module T$(Q : sig val q : int end) = struct
  module G (P : sig val a : int end) = struct
    module H = F$(P)
    module M = H$(struct let b = Q.q end)
    module K$(Z : sig val c : int end) = struct let w = Z.c + P.a + Q.q end
    module N = K$(struct let c = 100 end)
  end
  module G1 = G (struct let a = 1 end)
  module G2 = G (struct let a = 2 end)
end
module T1 = T$(struct let q = 7 end)
let () = Printf.printf "%d %d %d\n" T1.G1.M.v T1.G2.M.v T1.G1.N.w
