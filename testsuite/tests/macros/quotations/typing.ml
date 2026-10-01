(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A quotation of an expression of type t has type t expr. *)

let a = << 1 >>
let b = << fun x -> x >>
let c = << let y = 1 in y + y >>

(* A splice of an expression of type t expr has type t. *)

let d = $(<< 1 >>)
let e = $(<< fun x -> x + 1 >>) 2

(* Quotations and splices nest through each other. *)

let f = << 1 + $(<< 2 >>) >>
let g = $(let c = << 2 >> in << $c + $c >>)

(* A variable is used at the level at which it is bound. *)

let h x = $(<< x + 1 >>)
let i = << fun x -> $(<< x >>) >>
let j = $(let twice c = << $c + $c >> in twice << 1 >>)

(* Primitives, types and constructors belong to no level. *)

type t = A | B of int
let k = << B (1 + 2) >>
let l = << (A : t) >>
let m = $(let n = 1 + 2 in ignore n; << 3 >>)
