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

(* Where an environment-carrying macro may be CALLED from, within one
   compilation unit.  Each route has to name the environment slot
   differently -- locally, through an aliased module, through an
   include's re-export, through an enclosing macro's own environment --
   so each is a separate way of getting the address wrong. *)

let helper x = x + 1

macro gen () = << helper 5 >>

(* --- Two splices in the SAME structure item. *)

let a1 = $(gen ()) and a2 = $(gen ()) * 10

(* --- A later item, and a splice nested in a bigger expression. *)

let b1 = List.fold_left ( + ) 0 [ $(gen ()); $(gen ()); 1 ]

(* --- From inside another macro of this module (chaining: the callee's
   environment rides in the caller's). *)

macro outer () = << 1000 + $(gen ()) >>

let c1 = $(outer ())

(* Two levels of chaining. *)

macro outermost () = << 10000 + $(outer ()) >>

let c2 = $(outermost ())

(* --- A submodule's macro, called from the enclosing module. *)

module Sub = struct
  let sub_helper x = x * 3
  macro sub_gen () = << sub_helper 4 >>
end

let d1 = $(Sub.sub_gen ())

(* --- A SIBLING submodule's macro, called from inside another
   submodule's macro: the callee's environment slot lives in a local
   module's block, which the caller cannot name from where its code
   ends up, so it must ride in the caller's environment. *)

module Other = struct
  macro calls_sibling () = << 100 + $(Sub.sub_gen ()) >>
end

let d2 = $(Other.calls_sibling ())

(* And a submodule macro calling a macro of the ENCLOSING module. *)

module Third = struct
  macro calls_outer () = << 20000 + $(gen ()) >>
end

let d3 = $(Third.calls_outer ())

(* --- Through a module alias. *)

module AliasOfSub = Sub

let e1 = $(AliasOfSub.sub_gen ())

module AliasOfAlias = AliasOfSub

let e2 = $(AliasOfAlias.sub_gen ())

(* --- Through [open]. *)

open Sub

let f1 = $(sub_gen ())

(* --- Through [include] of a named local module: the include re-exports
   the environment slot positionally in both blocks. *)

module Included = struct
  let inc_helper x = x - 1
  macro inc_gen () = << inc_helper 50 >>
  macro inc_closed () = << 9 >>
end

include Included

let g1 = $(inc_gen ())
let g2 = $(inc_closed ())

(* --- Through [include struct ... end]. *)

include struct
  let lit_helper x = x * 7
  macro lit_gen () = << lit_helper 6 >>
end

let g3 = $(lit_gen ())

(* --- From a splice inside a mixed functor body, where the callee is a
   macro of the enclosing unit (its environment is an absolute address
   of this unit) as well as one of the body itself. *)

module F (X : sig val v : int end) = struct
  let scaled = X.v * 2
  macro body_gen () = << X.v + scaled >>
  let from_body = $(body_gen ())
  let from_outer = $(gen ())
end

module FA = F (struct let v = 10 end)
module FB = F (struct let v = 100 end)

(* --- From a splice inside a template functor body. *)

module T [Y : sig val u : int end] = struct
  let based = Y.u + helper 0
  macro tem_gen () = << Y.u * 3 + based >>
  let inside = $(tem_gen ())
  let outer_here = $(gen ())
end

module TA = T [struct let u = 5 end]
module TB = T [struct let u = 8 end]

let () =
  Printf.printf "same-item: %d %d\n" a1 a2;
  Printf.printf "later: %d\n" b1;
  Printf.printf "chained: %d %d\n" c1 c2;
  Printf.printf "submodule: %d %d %d\n" d1 d2 d3;
  Printf.printf "alias: %d %d\n" e1 e2;
  Printf.printf "open: %d\n" f1;
  Printf.printf "include: %d %d %d\n" g1 g2 g3;
  Printf.printf "mixed-functor: %d %d %d %d\n"
    FA.from_body FB.from_body FA.from_outer FB.from_outer;
  Printf.printf "template-functor: %d %d %d %d\n"
    TA.inside TB.inside TA.outer_here TB.outer_here
