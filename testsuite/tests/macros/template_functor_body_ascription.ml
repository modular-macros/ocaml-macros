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

(* An ASCRIBED template functor body containing in-body applications
   (frex bug 1; the trail is in git history): the chain pass must peel the constraint to the
   structure, composing the coercion into the record block, so the
   dynamic component's pseudo-thunks stay inside the chain's pair
   bindings -- losing the continuation left *tapppair* unbound in
   F_fn.  Covers the frex shape verbatim (value-typed macro), an
   ascription that hides body components over an in-body application
   spliced downstream (with a sharing constraint for observability),
   and the unascribed control. *)

(* the frex shape verbatim: value-typed macro through an ascribed
   template body (compile-only content) *)
module type S0 = sig type t macro c : t -> t end
module type W0 = sig module A : S0 end
module Sp0 [M : S0] = struct
  type t = M.t
  macro c : t -> t = fun x -> M.c x
end
module E0 [M : S0] : W0 = struct
  module A = Sp0[M]
end

(* observable: an ascription HIDING body components, over an in-body
   application whose macros splice downstream *)
module type S = sig type t macro c : t expr -> t expr end
module type W = sig module A : S end
module Sp [M : S] = struct
  type t = M.t
  macro c x = M.c x
end
module E [M : S] : W with type A.t = M.t = struct
  module A = Sp[M]
  macro hidden () = << 0 >>
  let dead = 42
end
module I1 = struct type t = int macro c x = << $x + 1 >> end
module I2 = struct type t = int macro c x = << $x * 2 >> end
module E1 = E[I1]
module E2 = E[I2]
let v1 = $( E1.A.c << 41 >> )
let v2 = $( E2.A.c << 10 >> )

(* unascribed control *)
module Eu [M : S] = struct module A = Sp[M] end
module EU = Eu[I1]
let v3 = $( EU.A.c << 6 >> )
let () = Printf.printf "%d %d %d\n" v1 v2 v3
