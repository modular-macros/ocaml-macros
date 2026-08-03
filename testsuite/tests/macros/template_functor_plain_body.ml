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

(* A template application inside a PLAIN functor body
   (BODY-MACRO-LIFT.md 7.3-7.4).  Level -1 effects track the
   compile-time world's evaluation structure: the application's
   compile-time part runs once per unit, at the functor's DEFINITION
   item, while the level 0 fragment instantiates per functor
   application through the environment pair -- the parameter is a
   binder of the emitted body, in scope at the fragment's hole. *)

module F[X : sig val base : int end] = struct
  let v = X.base * 2
  let w = $( << X.base + 1 >> )
end

module H (Y : sig val base : int end) = struct
  module M = F[Y]
  let s = M.v + M.w
end

module A = H (struct let base = 10 end)
module B = H (struct let base = 100 end)

(* An outside path with macros as the argument: the body splice's
   value goes through V's macro. *)
module F2[X : sig val base : int macro gen : unit -> int expr end] = struct
  let v = X.base + 1
  let w = $( X.gen () ) + v
end

module V = struct
  let base = 5
  macro gen () = << 40 >>
end

module H2 (Y : sig val flag : bool end) = struct
  module M = F2[V]
  let s = if Y.flag then M.w else M.v
end

module C = H2 (struct let flag = true end)
module D = H2 (struct let flag = false end)

let () =
  Printf.printf "%d %d %d %d\n" A.s B.s C.s D.s

(* Body-local arguments (lifted 2026-07-26): the argument may be
   rooted at a module the body itself defines.  The hoisted call never
   reads it -- a body-local cannot have macro components, so unit is
   passed -- and the fragment reaches it lexically at the hole,
   exactly like a parameter.  Shapes: a coerced local, a projection
   through a local, include, anonymous, a literal argument over
   locals, and an argument depending on an earlier application's
   result. *)
module H3 (Y : sig val base : int end) = struct
  module W = struct let base = Y.base + 3 let extra = 99 end
  module M = F[W]                     (* coercion drops [extra] *)
  module Deep = struct module Inner = struct let base = M.v end end
  module M2 = F[Deep.Inner]           (* chained, via a projection *)
  include F[W]
  module _ = F[W]
  module L = F[struct let base = Y.base + W.base end]
  let s = M.v + M.w + M2.v + M2.w + v + w + L.v + L.w
end

module E = H3 (struct let base = 10 end)
module G = H3 (struct let base = 100 end)

(* The same name at two nesting depths: each hole closes over its own
   binder by stamp, and an application inside a nested structure sits
   under that structure's bindings. *)
module H4 (Y : sig val base : int end) = struct
  module W = struct let base = Y.base + 1 end
  module Sub = struct
    module W = struct let base = Y.base + 2 end
    module M = F[W]
  end
  module M1 = F[W]
  let s = M1.v + Sub.M.v
end

module K = H4 (struct let base = 10 end)

let () =
  Printf.printf "%d %d %d\n" E.s G.s K.s
