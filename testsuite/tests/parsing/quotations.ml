(* TEST
 flags = "-dsource -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Quotations and splices.  This test only exercises the syntax. *)

let a = << 1 >>
let b = << f x; g y >>
let c = << let y = 1 in y >>
let d = <<x>>

let e = $(f x)
let f = $(f x; g y)
let g = $x
let h = $M.x
let i = $M.N.x

(* Quotations and splices are simple expressions. *)

let j = f << x >> $y $(z)
let k = $f y
let l = $a b c
let m = $x :: y
let n = $x.y
let o = $x#m
let p = $x.(0)
let q = $x.y <- 1
let r = $(x.y)

let s = << $x + $(f << y >>) >>

(* A dollar that is not glued to an identifier or to a parenthesis is
   an operator character. *)

let ( $ ) f x = f x
let t = f $ x
let u = f $ $x
let ( $$ ) f x = f x
let ( $: ) f x = f x
let ( <<< ) f g x = f (g x)
let ( >>= ) x f = f x
