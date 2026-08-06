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

(* Signatures that hide what a macro captures.  The environment holds the
   VALUE, so hiding should make no difference to a caller -- but the two
   blocks have to keep a shared layout across each coercion, or the
   caller reads the wrong slot.  [macro_env_hidden] does the unit-.mli
   case; here the restriction is a submodule ascription, a chain of two
   of them, and a functor result signature. *)

(* --- One restricting signature on a submodule. *)

module M : sig macro gen : unit -> int expr end = struct
  let hidden x = x + 1
  macro gen () = << hidden 5 >>
end

let a = $(M.gen ())

(* --- The signature keeps some run-time components and drops others, so
   the coercion reorders as well as narrowing. *)

module N : sig
  val kept : int
  macro gen : unit -> int expr
  val also_kept : string
end = struct
  let dropped_first = 1000
  let kept = 11
  let hidden x = x * 4
  macro gen () = << hidden dropped_first + hidden 1 >>
  let dropped_second = "no"
  let also_kept = "yes"
end

let b = $(N.gen ())

(* --- A chain of two signatures: the inner module is already restricted,
   and the module that re-exports it is restricted again. *)

module type Gen = sig macro gen : unit -> int expr end

module Inner : Gen = struct
  let secret x = x * 7
  macro gen () = << secret 6 >>
end

module Mid : sig module I : Gen end = struct
  module I = Inner
  let also_secret = 5
  macro unused () = << also_secret >>
end

let c = $(Mid.I.gen ())

(* Three deep. *)

module Outer : sig module Mi : sig module I : Gen end end = struct
  module Mi = Mid
end

let d = $(Outer.Mi.I.gen ())

(* --- The hiding signature is applied to a module the caller reaches
   through an alias. *)

module AliasOfM = M

let e = $(AliasOfM.gen ())

(* --- A functor result signature that hides the body's run-time
   bindings, including the functor parameter's projection. *)

module F (X : sig val v : int end) : sig macro m : unit -> int expr end =
struct
  let doubled = X.v * 2
  macro m () = << X.v + doubled >>
end

module FA = F (struct let v = 10 end)
module FB = F (struct let v = 100 end)

let f1 = $(FA.m ())
let f2 = $(FB.m ())

(* --- A template functor whose result signature hides the captured
   binding. *)

module type TGen = sig macro tg : unit -> int expr end

module T [Y : sig val u : int end] : TGen = struct
  let tripled = Y.u * 3
  macro tg () = << Y.u + tripled >>
end

module TA = T [struct let u = 5 end]
module TB = T [struct let u = 50 end]

let g1 = $(TA.tg ())
let g2 = $(TB.tg ())

(* --- Hiding a macro's captured MODULE: the signature drops the
   submodule the returned code projects from. *)

module P : sig macro pg : unit -> int expr end = struct
  module Hidden = struct let h = 31 end
  macro pg () = << Hidden.h + 1 >>
end

let h = $(P.pg ())

let () =
  Printf.printf "submodule: %d\n" a;
  Printf.printf "reordered: %d\n" b;
  Printf.printf "chain: %d %d\n" c d;
  Printf.printf "alias: %d\n" e;
  Printf.printf "functor-result: %d %d\n" f1 f2;
  Printf.printf "template-result: %d %d\n" g1 g2;
  Printf.printf "hidden-module: %d\n" h
