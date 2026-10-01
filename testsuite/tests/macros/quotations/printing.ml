(* TEST
 expect;
*)

(* The toplevel prints a code value as its term.  Identifiers are printed
   by name; distinct identifiers that share a name are numbered, in the
   order in which they first appear. *)

let a = << fun x -> x + 1 >>
let b = << fun x -> (fun x -> x + 1) (x * 2) >>
let c = << let x = 1 in let x = x + 1 in x >>
;;
[%%expect{|
val a : (int -> int) expr = <code (function x (+ x 1))>
val b : (int -> int) expr =
  <code (function x (apply (function x_1 (+ x_1 1)) (* x 2)))>
val c : int expr = <code (let (x = 1 x_1 = (+ x 1)) x_1)>
|}]

(* The [x] that the splice mentions is the outer one: quotations are
   hygienic, and the numbering shows it. *)

let d = << fun x -> $(let c = << x >> in << fun x -> $c + x >>) >>
;;
[%%expect{|
val d : (int -> int -> int) expr =
  <code (function x (function x_1 (+ x x_1)))>
|}]

(* A number never makes two identifiers look the same. *)

let e = << fun x -> fun x -> fun x_1 -> x + x_1 >>
;;
[%%expect{|
val e : ('a -> int -> int -> int) expr =
  <code (function x (function x_1 (function x_1_1 (+ x_1 x_1_1))))>
|}]

(* A splice's code is printed where it is placed. *)

let twice f = << fun x -> $(f << x >>) + $(f << x >>) >>
let f = twice (fun e -> << $e * 2 >>)
;;
[%%expect{|
val twice : ('a expr -> int expr) -> ('a -> int) expr = <fun>
val f : (int -> int) expr = <code (function x (+ (* x 2) (* x 2)))>
|}]

(* The kinds of values that the term records for code generation are not
   printed, in bindings, parameters, results or recursive functions. *)

let g = << let rec f n = if n = 0 then 1 else n * f (n - 1) in f 10 >>
let h = << fun f -> try f () with e -> raise e >>
;;
[%%expect{|
val g : int expr =
  <code
    (letrec (f (function n (if (== n 0) 1 (* n (apply f (- n 1))))))
      (apply f 10))>
val h : ((unit -> 'a) -> 'a) expr =
  <code (function f (try (apply f 0) with e (reraise e)))>
|}]

(* Code values inside other values, and code whose variable has escaped
   its binder. *)

let l = [ << 1 >>; << 2 >> ]
let s = Some << "a\n" >>
let r = ref (Expr.int 0)
let k = << fun x -> $(r := << x + 1 >>; << x >>) >>
let escaped = !r
;;
[%%expect{|
val l : int expr list = [<code 1>; <code 2>]
val s : string expr option = Some <code "a\n">
val r : int expr ref = {contents = <code 0>}
val k : (int -> int) expr = <code (function x x)>
val escaped : int expr = <code (+ x 1)>
|}]
