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

(* A template application inside a PLAIN functor body nested under a
   TEMPLATE functor body.  The inner application's chain -- the
   component-call pair -- floats to the top of the enclosing component
   function, evaluated once per outer instantiation (the plain-body
   hoisting semantics applied within the body world); the record
   projection inside the plain body and the pseudo-thunk's dyn call
   both scope under the floated binding.  v1 requires the inner
   functor and argument to be paths rooted outside both bodies. *)

module G[Y : sig val g : int end] = struct let gv = Y.g * 10 end
module V = struct let g = 7 end

module F[X : sig val v : int end] = struct
  module Plain (P : sig val p : int end) = struct
    module App = G[V]
    let r = App.gv + P.p + X.v
  end
end

module M = F[struct let v = 1 end]
module N1 = M.Plain (struct let p = 5 end)
module N2 = M.Plain (struct let p = 500 end)
let () = Printf.printf "nested: %d %d\n" N1.r N2.r

(* Two levels of plain nesting under the template body. *)
module F2[X : sig end] = struct
  module P1 (A : sig val a : int end) = struct
    module P2 (B : sig val b : int end) = struct
      module App = G[V]
      let r = App.gv + A.a + B.b
    end
  end
end
module M2 = F2[struct end]
module M21 = M2.P1 (struct let a = 100 end)
module M22 = M21.P2 (struct let b = 1000 end)
let () = Printf.printf "depth2: %d\n" M22.r
