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

(* A template application inside a functor body nested under a
   template body: the inner application's component-call pair is
   bound once per outer instantiation, in scope of the record
   projection inside the plain body and of the dynamic component's
   call.  The inner functor and argument are paths rooted outside both
   bodies. *)

module G$(Y : sig val g : int end) = struct let gv = Y.g * 10 end
module V = struct let g = 7 end

module F$(X : sig val v : int end) = struct
  module Plain (P : sig val p : int end) = struct
    module App = G$(V)
    let r = App.gv + P.p + X.v
  end
end

module M = F$(struct let v = 1 end)
module N1 = M.Plain (struct let p = 5 end)
module N2 = M.Plain (struct let p = 500 end)
let () = Printf.printf "nested: %d %d\n" N1.r N2.r

(* Two levels of plain nesting under the template body. *)
module F2$(X : sig end) = struct
  module P1 (A : sig val a : int end) = struct
    module P2 (B : sig val b : int end) = struct
      module App = G$(V)
      let r = App.gv + A.a + B.b
    end
  end
end
module M2 = F2$(struct end)
module M21 = M2.P1 (struct let a = 100 end)
module M22 = M21.P2 (struct let b = 1000 end)
let () = Printf.printf "depth2: %d\n" M22.r
