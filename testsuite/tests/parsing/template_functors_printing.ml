(* TEST
 flags = "-dsource -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Check that Pprintast emits templates in a form that parses
   back. *)

module type S = sig type t end

module F1$(X : S) = struct type t = X.t end
module F2$(_ : S) = struct type t = int end
module F3$() = struct type t = int end
module F4(X : S)$(Y : S) = struct type t = X.t * Y.t end

module A1 = F1$(struct type t = int end)
module A2 = F3$()
module A3 = F1$(F1$(struct type t = int end))

module type T1 = $(X : S) -> S
module type T2 = $(S) -> S
module type T3 = $() -> S
module type T4 = $($(S) -> S) -> S
module type T5 = (X : S) -> $(Y : S) -> S

(* A template type used as the domain of a plain arrow has to be
   printed parenthesized: without the parentheses "$S -> S -> S" would
   parse back as "$S -> (S -> S)". *)
module type T6 = ($(S) -> S) -> S

(* A parameter that is not named and whose type is a name takes the
   short form; any other type keeps its parentheses. *)
module M = struct module type S = S end
module type T7 = $S -> $M.S -> S
module type T8 = $(S with type t = int) -> $(sig end) -> S
module type T9 = $(M.S) -> $(F1$(M).S) -> S

(* A module type may have a name that is not capitalised, and takes
   the short form too.  One whose name is a keyword is written with an
   escape, and keeps its parentheses unless it is qualified. *)
module type s = S
module type \#let = S
module N = struct module type s = S  module type \#val = S end
module type T10 = $(s) -> $(N.s) -> $s -> $N.s -> S
module type T11 = $(\#let) -> $(N.\#val) -> $N.\#val -> S
