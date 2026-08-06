(* TEST
 readonly_files = "toplevel_mixed_forms_lib.ml";
 setup-ocamlc.byte-build-env;
 module = "toplevel_mixed_forms_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I . toplevel_mixed_forms_lib.cmo toplevel_mixed_forms_lib\$macros.cmo";
 ocaml;
 check-ocaml-output;
*)

(* Toplevel mixed functors: the residual forms (TOPLEVEL.md phase 3
   coda, lifted 2026-07-31).  A NAMED phrase structure with mixed
   functors however nested compiles as a phase-4 two-world region --
   run block under its name, the stage -1 block under the record key
   -- with stage -1 member paths rerooted onto the record;
   applications accept IMMEDIATE and GENERATIVE functors; [include
   F(V)] rebinds each macro component as a table PAIR (function from
   the compile-time block, environment from the run-time one), which
   serves includes of compiled units' macros by the same mechanism; a
   TEMPLATE application's result may carry a mixed functor member.
   The still-rejected corners close the file: anonymous mixed
   structures, [open F(V)], and includes that would rebind a
   mixed-functor MEMBER positionally (its shared-block key cannot
   travel with a field copy). *)

(* Nested in a phrase structure: definition beside a direct macro,
   deep nesting, an in-structure application of the sibling. *)
module Outer = struct
  macro pm e = << $e + 1000 >>
  module F (X : sig val v : int end) = struct
    macro m e = << $e + X.v >>
    let run = 100 + X.v
  end
  module Inner = struct
    module G (X : sig val v : int end) = struct
      macro m e = << $e * X.v >>
    end
  end
  module M0 = F (struct let v = 6 end)
end
;;

module M = Outer.F (struct let v = 7 end)
;;
let a = $(M.m << 1 >>) + $(Outer.pm << 1 >>) + M.run
;;
module MD = Outer.Inner.G (struct let v = 3 end)
;;
let b = $(MD.m << 5 >>) + $(Outer.M0.m << 7 >>) + Outer.M0.run
;;

(* Aliases: of the nested functor, and of the structure. *)
module GF = Outer.F
;;
module G2 = Outer
;;
module M2 = GF (struct let v = 20 end)
;;
module M3 = G2.F (struct let v = 30 end)
;;
let c = $(M2.m << 2 >>) + $(M3.m << 3 >>)
;;

(* Same-phrase definition and application. *)
module Outer2 = struct
  module F (X : sig val v : int end) = struct
    macro m e = << $e + X.v >>
  end
end
module M4 = Outer2.F (struct let v = 40 end)
;;
let d = $(M4.m << 4 >>)
;;

(* A sealed phrase structure. *)
module Outer3 : sig
  module F (X : sig val v : int end) : sig
    macro m : int expr -> int expr
  end
end = struct
  module F (X : sig val v : int end) = struct
    macro m e = << $e + X.v >>
    let hidden = 99
  end
end
;;
module M5 = Outer3.F (struct let v = 50 end)
;;
let e = $(M5.m << 5 >>)
;;

(* Immediate and generative applications. *)
module MI = (functor (X : sig val v : int end) -> struct
  macro m e = << $e + X.v >>
  let run = 200 + X.v
end) (struct let v = 4 end)
;;
module FG () = struct macro g e = << $e + 42 >> end
;;
module MG = FG ()
;;
let f = $(MI.m << 1 >>) + MI.run + $(MG.g << 1 >>)
;;

(* include F(V): the short names are table pairs; a same-phrase splice
   and a later one both serve. *)
module F6 (X : sig val v : int end) = struct
  macro m6 e = << $e + X.v >>
  let w6 = 10 * X.v
end
;;
include F6 (struct let v = 7 end)
;;
let g = $(m6 << 1 >>) + w6
;;
module F7 (X : sig val v : int end) = struct
  macro m7 e = << $e * X.v >>
end
include F7 (struct let v = 4 end)
let h = $(m7 << 5 >>)
;;
include (functor (X : sig val v : int end) -> struct
  macro m8 e = << $e - X.v >>
end) (struct let v = 3 end)
;;
let i = $(m8 << 10 >>)
;;

(* include of a compiled unit's macros: the same pair assembly. *)
include Toplevel_mixed_forms_lib
;;
let j = $(bm << 1 >>) + bv
;;

(* A template application's result carrying a mixed functor member;
   projection, alias, independent instantiations. *)
module T [X : sig val b : int end] = struct
  let base = X.b
  module G (Y : sig val z : int end) = struct
    macro gm e = << $e + Y.z + X.b >>
    let gr = Y.z * 100
  end
end
;;
module TA = T [struct let b = 5 end]
;;
module MT = TA.G (struct let z = 2 end)
;;
module GA = TA.G
;;
module MT2 = GA (struct let z = 9 end)
;;
module TB = T [struct let b = 50 end]
;;
module MT3 = TB.G (struct let z = 1 end)
;;
let k = $(MT.gm << 1 >>) + MT.gr + $(MT2.gm << 2 >>) + $(MT3.gm << 0 >>)
;;

Printf.printf "forms: a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d i=%d j=%d k=%d\n"
  a b c d e f g h i j k
;;

(* Still rejected, each with the located error: an anonymous mixed
   structure, [open F(V)], and an include that would rebind a
   mixed-functor member. *)
module _ = struct
  module F (X : sig val v : int end) = struct
    macro m e = << $e + X.v >>
  end
end
;;
open F6 (struct let v = 1 end)
;;
include Outer
;;
