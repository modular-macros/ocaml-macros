(* TEST
 flags = "-dparsetree -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Templates are written with a dollar before the parentheses used
   by functors.  This test only exercises the syntax: the type checker
   rejects all of these (see tests/macros/template_functors.ml). *)

module type S = sig type t end

(* Definitions. *)

module F1$(X : S) = struct type t = X.t end
module F2 = functor $(X : S) -> struct type t = X.t end

(* An anonymous parameter, as in "(_ : S)". *)
module F3$(_ : S) = struct type t = int end

(* A unit parameter, as in "()": "$()" is two tokens. *)
module F4$() = struct type t = int end
module F5 = functor $() -> struct type t = int end

(* The two kinds of parameter may be mixed in one curried definition. *)
module F6(X : S)$(Y : S) = struct type t = X.t * Y.t end
module F7$(X : S)(Y : S) = struct type t = X.t * Y.t end

(* Applications.  The argument is delimited already, so it need not be
   parenthesized again. *)

module A1 = F1$(struct type t = int end)
module A2 = F4$()
module A3 = F1$((struct type t = int end : S))

(* Nesting needs no spaces. *)
module A4 = F1$(F1$(struct type t = int end))
module A5 = F6(struct type t = int end)$(struct type t = int end)

(* Application associates to the left, so A6 is "(F1$(M))[N]". *)
module A6 = F1$(struct type t = int end)$(struct type t = int end)

(* Template types.  Unlike "(X : A) -> B", these have no arrow. *)

module type T1 = $(X : S) -> S
module type T2 = $(_ : S) -> S
module type T3 = $(S) -> S
module type T4 = $() -> S

(* The body extends as far to the right as possible, so T5 is
   "[S] (S -> S)" rather than "([S] S) -> S". *)
module type T5 = $(S) -> S -> S

module type T6 = (X : S) -> $(Y : S) -> S
module type T7 = $(X : S) -> (Y : S) -> S

module type T8 = sig
  module F$(X : S) : S
  module G$() : S
  module H(X : S)$(Y : S) : S
end

(* The short form of a parameter that is not named, for a type that is
   a name. *)
module type T10 = $S -> S
module type T11 = $M.S -> $S -> (X : S) -> S -> S
