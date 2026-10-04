(* TEST
 readonly_files = "coerced_alias_member_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "coerced_alias_member_lib.ml \
     coerced_alias_member.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "coerced_alias_member_lib.ml \
     coerced_alias_member.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A template with an alias member, [module M = K], coerced where it is
   not defined: as a member of a signature, and as the argument of
   another template.  The coercion of the member names K; the code of
   an application is coerced as code, which used to mention K as a bare
   variable that the static program read as its own, taking a
   compile-time block for an identifier.  The coerced template now
   carries K in its environment, so the application may be in another
   unit, as the first is here. *)

module Lib = Coerced_alias_member_lib

module P = Lib.HI.F$(Lib.IntS)
module Q = Lib.HI.F$(Lib.IntS)

(* In one unit, the coercion at the passing of an argument. *)
module type FT = $(X : Lib.Setoid) -> sig
  module M : sig $val k : int  val v : int end
end
module K = struct $let k : int = 9 $let other : int = 8 let w = 1 let v = 3 end
module Id1 $(X : Lib.Setoid) = struct type t = X.t module M = K end
module G $(F : FT) = struct
  module A = F$(Lib.IntS)
  let r = $(Expr.int A.M.k) + A.M.v
end
module R = G$(Id1)

(* Curried templates: every level carries the names. *)
module CX = Lib.HC1.F$(Lib.IntS)
module C = CX$(Lib.IntS)
module CG = C.G$(Lib.IntS)
module DX = Lib.HC3.F$(Lib.IntS)
module D = DX$(Lib.IntS)

let () =
  Printf.printf "%s %d %d %d %d %d\n"
    $(Expr.string (P.show (Obj.magic 3))) $(Expr.int P.M.k) P.M.v
    $(P.M.g << 1 >>) Q.M.v R.r;
  Printf.printf "%d %d %d %d %d\n"
    $(Expr.int C.M.k) C.M.v CG.N.y $(Expr.int CG.N.j) D.M.v
