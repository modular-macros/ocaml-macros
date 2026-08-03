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

(* Template applications inside plain functor bodies nested under
   template functor bodies (the 2026-08 lift): the outside-both-bodies
   restriction is gone.  Each application's chain pair binds at its
   plain body's definition item -- once per outer instantiation, in
   declaration order -- so the component call reaches the template
   body's earlier bindings and its parameters; the environment pair's
   roots are DELIVERED at the marker hole, so the argument may be
   rooted at the template body, at the template's parameters, at the
   plain body's parameters, or at its locals, evaluated per
   application of the plain functor.  Pinned here: each root class,
   per-outer generativity, per-inner-application environment reads, a
   macro-bearing template-body argument, deep nesting (the chain
   collects at the OUTERMOST plain body's item), a bare-functor
   template body (the fallback chain), and the previously accepted
   outside-rooted shape. *)

(* Argument rooted at a template-body binding; per-outer
   generativity. *)
module Show [P : sig val label : string end] = struct
  macro k () = << 10 >>
  let w = $(k ())
  let show n = P.label ^ ": " ^ string_of_int n
end
module T [X : sig val tag : string end] = struct
  module Local = struct let label = X.tag ^ "!" end
  module F (W : sig val n : int end) = struct
    module S = Show [Local]
    let out = S.show (W.n + S.w)
  end
end
module A = T [struct let tag = "left" end]
module MA1 = A.F (struct let n = 3 end)
module MA2 = A.F (struct let n = 4 end)
module B = T [struct let tag = "right" end]
module MB = B.F (struct let n = 5 end)
let () = print_endline ("p1a " ^ MA1.out)
let () = print_endline ("p1b " ^ MA2.out)
let () = print_endline ("p1c " ^ MB.out)

(* Functor rooted at a template-body definition; argument at the
   template's parameter. *)
module T2 [X : sig val v : int end] = struct
  module H [Y : sig val v : int end] = struct
    let hv = Y.v * 10
  end
  module F (W : sig val m : int end) = struct
    module S1 = H [X]
    let out = S1.hv + W.m
  end
end
module C = T2 [struct let v = 7 end]
module MC = C.F (struct let m = 2 end)
let () = print_endline ("p2 " ^ string_of_int MC.out)

(* Arguments at the plain body's parameter and at a body-local, read
   per application of the plain functor. *)
module Show3 [P : sig val q : int end] = struct
  let sq = P.q * P.q
end
module T3 [X : sig val a : int end] = struct
  module F (W : sig val q : int end) = struct
    module Loc = struct let q = W.q + X.a end
    module S1 = Show3 [W]
    module S2 = Show3 [Loc]
    let out = S1.sq + S2.sq
  end
end
module D = T3 [struct let a = 1 end]
module MD = D.F (struct let q = 3 end)
module MD2 = D.F (struct let q = 10 end)
let () = print_endline ("p3a " ^ string_of_int MD.out)
let () = print_endline ("p3b " ^ string_of_int MD2.out)

(* Two applications in one body: both serve, declaration order. *)
module U [P : sig val x : int end] = struct
  let a = P.x + 1
end
module V [P : sig val x : int end] = struct
  let b = P.x + 2
end
module T4 [X : sig val x : int end] = struct
  module F (W : sig end) = struct
    module S1 = U [X]
    module S2 = V [X]
    let out = S1.a * 100 + S2.b
  end
end
module E4 = T4 [struct let x = 20 end]
module ME4 = E4.F (struct end)
let () = print_endline ("p4 " ^ string_of_int ME4.out)

(* A macro-bearing template-body module as the argument: the
   component call reads its stage -1 block at the sunk item. *)
module Use [P : sig macro g : unit -> int expr val h : int end] = struct
  let r = $(P.g ()) + P.h
end
module TM [X : sig val a : int end] = struct
  module MacMod = struct
    macro g () = << 7 >>
    let h = 100
  end
  module F (W : sig val w : int end) = struct
    module S = Use [MacMod]
    let out = S.r + W.w
  end
end
module E = TM [struct let a = 0 end]
module ME = E.F (struct let w = 1 end)
let () = print_endline ("p5 " ^ string_of_int ME.out)

(* Deep nesting: the argument at the MIDDLE functor's parameter; the
   chain collects at the outermost plain body's item. *)
module Show5 [P : sig val z : int end] = struct let out = P.z + 1 end
module T5 [X : sig val b : int end] = struct
  module Mid (M : sig val z : int end) = struct
    module Deep (N : sig val u : int end) = struct
      module S = Show5 [M]
      let r = S.out + N.u + X.b
    end
  end
end
module E5 = T5 [struct let b = 1000 end]
module ME5 = E5.Mid (struct let z = 5 end)
module MED = ME5.Deep (struct let u = 100 end)
let () = print_endline ("p6 " ^ string_of_int MED.r)

(* A bare-functor template body: the component-function-top fallback
   chain, outside roots only. *)
module Out6 = struct let c = 3 end
module Show6 [P : sig val c : int end] = struct let d = P.c * 2 end
module T6 [X : sig val k : int end] = functor (W : sig val e : int end) ->
struct
  module S = Show6 [Out6]
  let out = S.d + W.e + X.k
end
module E6 = T6 [struct let k = 100 end]
module ME6 = E6 (struct let e = 30 end)
let () = print_endline ("p7 " ^ string_of_int ME6.out)

(* The previously accepted outside-rooted shape. *)
module Arg8 = struct let label = "outer" end
module T8 [X : sig val t : int end] = struct
  module F (W : sig end) = struct
    module S = Show [Arg8]
    let out = S.show X.t
  end
end
module E8 = T8 [struct let t = 9 end]
module ME8 = E8.F (struct end)
let () = print_endline ("p8 " ^ ME8.out)

(* Literals in the nested position (lifted 2026-07-31): a value-only
   literal argument referencing both bodies' binders, evaluated per
   application of the plain functor; a literal functor, including one
   with its own body macro and splice. *)
module G9 [Y : sig val g : int end] = struct let a = Y.g * 2 end
module T9 [X : sig val b : int end] = struct
  module Local9 = struct let x = 100 end
  module F (W : sig val n : int end) = struct
    module S = G9 [struct let g = W.n + Local9.x + X.b end]
    let out = S.a
  end
end
module A9 = T9 [struct let b = 1000 end]
module M9a = A9.F (struct let n = 3 end)
module M9b = A9.F (struct let n = 4 end)
let () = print_endline ("p9a " ^ string_of_int M9a.out)
let () = print_endline ("p9b " ^ string_of_int M9b.out)
module T10 [X : sig val c : int end] = struct
  module F (W : sig end) = struct
    module S = (functor [Y : sig val g : int end] -> struct
      macro k () = << 3 >>
      let a = $(k ()) + Y.g
    end) [struct let g = 40 end]
    let out = S.a + X.c
  end
end
module A10 = T10 [struct let c = 100 end]
module M10 = A10.F (struct end)
let () = print_endline ("p10 " ^ string_of_int M10.out)

(* include F[V] in template bodies (lifted 2026-07-31), direct and
   nested: rebound names in later items and QUOTED in later splices,
   per-application evaluation, an included module as a later
   application's argument, and the ordinary-include quoted-name fix
   that fell out of counting include-bound names as body binders. *)
module G11 [Y : sig val g : int end] = struct
  let a = Y.g * 2
  let b = Y.g + 1
end
module V11 = struct let g = 5 end
module T11 [X : sig val t : int end] = struct
  include G11 [V11]
  let c = a + b + X.t
end
module A11 = T11 [struct let t = 100 end]
let () = print_endline ("p11a " ^ string_of_int A11.c)
module T12 [X : sig val t : int end] = struct
  module Arg = struct let g = X.t end
  module F (W : sig val w : int end) = struct
    include G11 [Arg]
    let d = a + W.w
    let q = $(<< b + 1 >>)
  end
end
module B12 = T12 [struct let t = 3 end]
module M12a = B12.F (struct let w = 10 end)
module M12b = B12.F (struct let w = 20 end)
let () = print_endline ("p11b " ^ string_of_int M12a.d)
let () = print_endline ("p11c " ^ string_of_int M12b.d)
let () = print_endline ("p11d " ^ string_of_int M12a.q)
module H13 [Y : sig val m : int end] = struct let hm = Y.m * 7 end
module GM13 [Y : sig val g : int end] = struct
  module Inner = struct let m = Y.g + 1 end
end
module T13 [X : sig end] = struct
  module F (W : sig end) = struct
    include GM13 [V11]
    module S2 = H13 [Inner]
    let e = S2.hm
  end
end
module C13 = T13 [struct end]
module M13 = C13.F (struct end)
let () = print_endline ("p12a " ^ string_of_int M13.e)
module T14 [X : sig val b : int end] = struct
  include struct let iv = 5 end
  let s2 = $(<< iv + 2 >>)
end
module D14 = T14 [struct let b = 1 end]
let () = print_endline ("p12b " ^ string_of_int D14.s2)

(* Compile-parts literal arguments in the nested position (lifted
   2026-07-31, the wrapper chain): a literal with a macro the
   template's body splice calls, one whose macro quotes the literal's
   own binding, one with its own top-level splice, and one containing
   its own template application. *)
module G20 [Y : sig macro m : unit -> int expr end] = struct
  let a = $(Y.m ())
end
module T20 [X : sig val b : int end] = struct
  module F (W : sig end) = struct
    module S = G20 [struct macro m () = << 42 >> end]
    module S2 = G20 [struct
      let base = 7
      macro m () = << base * 3 >>
    end]
    let out = S.a + X.b
    let out2 = S2.a
  end
end
module A20 = T20 [struct let b = 100 end]
module M20 = A20.F (struct end)
let () = print_endline ("p13a " ^ string_of_int M20.out)
let () = print_endline ("p13b " ^ string_of_int M20.out2)
module G21 [Y : sig val v : int end] = struct let a = Y.v * 2 end
module H21 [Z : sig val z : int end] = struct let hz = Z.z + 1 end
module V21 = struct let z = 3 end
module T21 [X : sig end] = struct
  module F (W : sig end) = struct
    module S = G21 [struct
      macro k () = << 5 >>
      let v = $(k ()) + 1
    end]
    module S2 = G21 [struct
      module Inner = H21 [V21]
      let v = Inner.hz * 10
    end]
    let out = S.a + S2.a
  end
end
module A21 = T21 [struct end]
module M21 = A21.F (struct end)
let () = print_endline ("p14 " ^ string_of_int M21.out)

(* Macro-bearing include results (lifted 2026-07-31): the included
   macro called by later body splices with per-instantiation
   environments, exported by the template result and used downstream
   through the record route, included in a NESTED (thereby mixed)
   plain body, and composed with a body macro in one splice. *)
module GM22 [Y : sig val g : int end] = struct
  macro mm () = << Y.g * 2 >>
  let v = Y.g
end
module T22 [X : sig val t : int end] = struct
  module Arg = struct let g = X.t end
  include GM22 [Arg]
  let w = $(mm ()) + v
end
module A22 = T22 [struct let t = 21 end]
module B22 = T22 [struct let t = 5 end]
let () = print_endline ("p15a " ^ string_of_int A22.w)
let () = print_endline ("p15b " ^ string_of_int B22.w)
module C22 = T22 [struct let t = 7 end]
let dw22 = $(C22.mm ()) + C22.v
let () = print_endline ("p15c " ^ string_of_int dw22)
module T23 [X : sig val t : int end] = struct
  module Arg = struct let g = X.t end
  module F (W : sig val u : int end) = struct
    include GM22 [Arg]
    let r = $(mm ()) + v + W.u
  end
end
module D23 = T23 [struct let t = 4 end]
module M23 = D23.F (struct let u = 100 end)
let () = print_endline ("p15d " ^ string_of_int M23.r)
module T24 [X : sig val t : int end] = struct
  include GM22 [struct let g = X.t end]
  macro own () = << 1 >>
  let s = $(<< $(mm ()) + $(own ()) >>)
end
module E24 = T24 [struct let t = 10 end]
let () = print_endline ("p15e " ^ string_of_int E24.s)

(* Module members with compile-time content through include (lifted
   2026-07-31, the final corner): the batch conventions are
   positional through the member's binder, so the two-world rebind
   -- the record's block at stage -1, the fragment's at stage 0 --
   is all a macro-bearing submodule, a template functor member, or a
   mixed functor member needs.  Pinned: each kind, in-body and
   downstream use, a nested host, a coercion that reorders the
   result's positions, and a member nested a level down. *)
module GS30 [Y : sig val g : int end] = struct
  module Sub = struct
    macro sm () = << 11 >>
    let x = Y.g
  end
end
module T30 [X : sig val t : int end] = struct
  include GS30 [struct let g = X.t end]
  let w = $(Sub.sm ()) + Sub.x
end
module A30 = T30 [struct let t = 5 end]
let () = print_endline ("p16a " ^ string_of_int A30.w)
module GT31 [Y : sig val g : int end] = struct
  module Tf [Z : sig val z : int end] = struct
    let tz = Z.z * 10 + Y.g
  end
end
module T31 [X : sig end] = struct
  include GT31 [struct let g = 7 end]
  module App = Tf [struct let z = 3 end]
  let v = App.tz
end
module B31 = T31 [struct end]
let () = print_endline ("p16b " ^ string_of_int B31.v)
module Down31 = B31.Tf [struct let z = 4 end]
let () = print_endline ("p16c " ^ string_of_int Down31.tz)
module GX32 [Y : sig val g : int end] = struct
  module MF (W : sig val w : int end) = struct
    macro m () = << W.w + Y.g >>
    let run = W.w * 2
  end
end
module T32 [X : sig end] = struct
  include GX32 [struct let g = 100 end]
  module M = MF (struct let w = 3 end)
  let s = $(M.m ()) + M.run
end
module C32 = T32 [struct end]
let () = print_endline ("p16d " ^ string_of_int C32.s)
module T33 [X : sig val t : int end] = struct
  module F (P : sig val p : int end) = struct
    include GS30 [struct let g = X.t end]
    let r = $(Sub.sm ()) + Sub.x + P.p
  end
end
module D33 = T33 [struct let t = 30 end]
module M33 = D33.F (struct let p = 200 end)
let () = print_endline ("p16e " ^ string_of_int M33.r)
module GC34 [Y : sig val g : int end] = struct
  let a = Y.g * 2
  macro cm () = << 9 >>
  let b = Y.g + 1
end
module T34 [X : sig end] = struct
  include (GC34 [struct let g = 5 end]
           : sig val b : int  macro cm : unit -> int expr end)
  let u = $(cm ()) + b
end
module E34 = T34 [struct end]
let () = print_endline ("p16f " ^ string_of_int E34.u)
module GD35 [Y : sig val g : int end] = struct
  module Outer = struct
    module Tf [Z : sig val z : int end] = struct let o = Z.z + Y.g end
  end
end
module T35 [X : sig end] = struct
  include GD35 [struct let g = 20 end]
  module App = Outer.Tf [struct let z = 1 end]
  let d = App.o
end
module F35 = T35 [struct end]
let () = print_endline ("p16g " ^ string_of_int F35.d)
