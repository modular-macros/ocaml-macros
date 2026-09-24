(* TEST
 flags = "-dsource -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Check that Pprintast emits template functors in a form that parses
   back. *)

module type S = sig type t end

module F1[X : S] = struct type t = X.t end
module F2[_ : S] = struct type t = int end
module F3[] = struct type t = int end
module F4(X : S)[Y : S] = struct type t = X.t * Y.t end

module A1 = F1[struct type t = int end]
module A2 = F3[]
module A3 = F1[F1[struct type t = int end]]

module type T1 = [X : S] S
module type T2 = [S] S
module type T3 = [] S
module type T4 = [[S] S] S
module type T5 = (X : S) -> [Y : S] S

(* A template functor type used as the domain of a plain arrow has to be
   printed parenthesized: without the parentheses "[S] S -> S" would
   parse back as "[S] (S -> S)". *)
module type T6 = ([S] S) -> S
