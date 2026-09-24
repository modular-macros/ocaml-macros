(* TEST
 expect;
*)

(* GADT equations used by code built inside a splice, checked by
   evidence.  An arm in quoted code binds evidence for the equations
   its pattern learned; a quotation that uses one of them carries the
   evidence among its free variables, so code that takes the equation
   out of the arm is caught when the generator runs, exactly as an
   escaping variable is.  The typing side: the equation is usable at
   the match's level and above, in nested quotations included, and not
   below.  The generation-time refusals are gadt_evidence_c*.ml; the
   accepted programs run in gadt_evidence_run.ml. *)

type (_,_) eql = Refl : ('a, 'a) eql
;;

[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
|}]

(* C17: the coercion inside the splice body, in a nested quotation,
   with the code landing in the arm. *)

let c17 : type a b. (a, b) eql expr -> (b expr -> b expr) -> (a -> b) expr =
  fun r f -> << fun x -> match $r with Refl -> $(f << (x : b) >>) >>
;;

[%%expect{|
val c17 : ('a, 'b) eql expr -> ('b expr -> 'b expr) -> ('a -> 'b) expr =
  <fun>
|}]

(* A splice of unrelated code at the refined type is still refused:
   the equation is used, not bypassed. *)

let hole : type a b. (a, b) eql expr -> int expr -> (a -> b) expr =
  fun r c -> << fun x -> match $r with Refl -> $c >>
;;

[%%expect{|
Line 2, characters 47-49:
2 |   fun r c -> << fun x -> match $r with Refl -> $c >>
                                                   ^^
Error: This expression has type "int expr"
       but an expression was expected of type "b expr"
       Type "int" is not compatible with type "b" = "a"
|}]

(* C19 and C20: a typecase in the generated program.  A code value's
   type is converted inside a quotation, << ($x : int) >>, where the
   equation is usable and the use is recorded; converting the value
   itself at the generator's level, fi x, is refused, since nothing
   there could carry the evidence. *)

type _ ty = Int : int ty | Str : string ty | Bool : bool ty

let typecase : type a r. a ty expr -> (int expr -> r expr) -> (string expr -> r expr)
                         -> (bool expr -> r expr) -> a expr -> r expr =
  fun r fi fs fb x ->
    << match $r with
       | Int -> $(fi << ($x : int) >>) | Str -> $(fs << ($x : string) >>)
       | Bool -> $(fb << ($x : bool) >>) >>
;;

[%%expect{|
type _ ty = Int : int ty | Str : string ty | Bool : bool ty
val typecase :
  'a ty expr ->
  (int expr -> 'r expr) ->
  (string expr -> 'r expr) -> (bool expr -> 'r expr) -> 'a expr -> 'r expr =
  <fun>
|}]

let typecase_value : type a r. a ty expr -> (int expr -> r expr) -> a expr -> r expr =
  fun r fi x -> << match $r with Int -> $(fi x) | Str -> $(fi x) | Bool -> $(fi x) >>
;;

[%%expect{|
Line 2, characters 45-46:
2 |   fun r fi x -> << match $r with Int -> $(fi x) | Str -> $(fi x) | Bool -> $(fi x) >>
                                                 ^
Error: The value "x" has type "a expr" but an expression was expected of type
         "int expr"
       Type "a" is not compatible with type "int"
|}]

let typecase_gen : type a r. a ty expr -> (r expr -> int expr) -> (r expr -> string expr)
                             -> (r expr -> bool expr) -> r expr -> a expr =
  fun r fi fs fb s ->
    << match $r with Int -> $(fi s) | Str -> $(fs s) | Bool -> $(fb s) >>
;;

[%%expect{|
val typecase_gen :
  'a ty expr ->
  ('r expr -> int expr) ->
  ('r expr -> string expr) -> ('r expr -> bool expr) -> 'r expr -> 'a expr =
  <fun>
|}]

(* C07 and C18 typecheck: the escape is caught when the generator
   runs (gadt_evidence_c07.ml, gadt_evidence_c18.ml). *)

let c07 : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell : b expr option ref = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    Option.get !cell
;;

[%%expect{|
val c07 : ('a, 'b) eql expr -> 'a expr -> 'b expr = <fun>
|}]

let c18 : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell : b expr option ref = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    << match $r with Refl -> $(Option.get !cell) >>
;;

[%%expect{|
val c18 : ('a, 'b) eql expr -> 'a expr -> 'b expr = <fun>
|}]

(* A function parameter's pattern learns equations too, and binds
   evidence like an arm. *)

let param : type a b. (a, b) eql expr -> (b expr -> b expr) -> (a -> b) expr =
  fun r f -> << fun x -> (fun (Refl : (a, b) eql) -> $(f << (x : b) >>)) $r >>
;;

[%%expect{|
val param : ('a, 'b) eql expr -> ('b expr -> 'b expr) -> ('a -> 'b) expr =
  <fun>
|}]
