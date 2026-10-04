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

(* An ascribed template body containing applications: the coercion
   applies to the record, and the applications' pairs stay in scope
   of the dynamic component.  A value-typed generator through an
   ascribed body, an ascription that hides body components over an
   application spliced downstream (with a sharing constraint, for
   observability), and the unascribed control. *)

(* A value-typed generator through an ascribed template body. *)
module type S0 = sig type t $val[..] c : t -> t end
module type W0 = sig module A : S0 end
module Sp0 $(M : S0) = struct
  type t = M.t
  $let[..] c : t -> t = fun x -> M.c x
end
module E0 $(M : S0) : W0 = struct
  module A = Sp0$(M)
end

(* observable: an ascription HIDING body components, over an in-body
   application whose macros splice downstream *)
module type S = sig type t $val[..] c : t expr -> t expr end
module type W = sig module A : S end
module Sp $(M : S) = struct
  type t = M.t
  $let[..] c x = M.c x
end
module E $(M : S) : W with type A.t = M.t = struct
  module A = Sp$(M)
  $let[..] hidden () = << 0 >>
  let dead = 42
end
module I1 = struct type t = int $let[..] c x = << $x + 1 >> end
module I2 = struct type t = int $let[..] c x = << $x * 2 >> end
module E1 = E$(I1)
module E2 = E$(I2)
let v1 = $( E1.A.c << 41 >> )
let v2 = $( E2.A.c << 10 >> )

(* unascribed control *)
module Eu $(M : S) = struct module A = Sp$(M) end
module EU = Eu$(I1)
let v3 = $( EU.A.c << 6 >> )
let () = Printf.printf "%d %d %d\n" v1 v2 v3
