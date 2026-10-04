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

(* A functor whose result is a template: its applications are
   templates, applied in turn.  The functor's compile-time value is
   the template's component function, which the applications share;
   the parameter is a root of the template's environment. *)

module F (Y : sig val u : int end) $(X : sig val v : int end) = struct
  let w = X.v + Y.u
  $let[..] g () = << w >>
end
module G1 = F (struct let u = 1 end)
module G2 = F (struct let u = 2 end)
module A = G1$(struct let v = 2 end)
module B = G2$(struct let v = 2 end)
module H (P : sig val v : int end) = struct module C = G1$(P) end
module H1 = H (struct let v = 5 end)
let () = Printf.printf "%d %d %d %d\n" A.w B.w H1.C.w $(A.g ())

(* Ascribed. *)
module type T =
  (Y : sig val u : int end) -> $(X : sig val v : int end) -> sig val w : int end
module F' : T =
  functor (Y : sig val u : int end) $(X : sig val v : int end) ->
    struct let w = X.v + Y.u let hidden = 1 end
module G' = F' (struct let u = 1 end)
module A' = G'$(struct let v = 2 end)
let () = Printf.printf "%d\n" A'.w

(* The functor application itself as the applied template, and a
   functor-result template applied in a functor body applied twice. *)
module M = F (struct let u = 200 end)$(struct let v = 1 end)
module K (P : sig val v : int end) = struct module C = G1$(P) module D = F (struct let u = P.v end)$(P) end
module K1 = K (struct let v = 1 end)
module K2 = K (struct let v = 2 end)
let () = Printf.printf "%d %d %d %d %d\n" M.w K1.C.w K2.C.w K1.D.w K2.D.w
