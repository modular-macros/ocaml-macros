(* TEST
 expect;
*)

(* An equation that a match on a GADT learns is not usable below the
   level of the match: a splice in the branch is evaluated before the
   match in the code that it builds. *)

type (_, _) eql = Refl : ('a, 'a) eql
;;
[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
|}]

let bad5 : type a b. (a, b) eql expr -> (b -> unit) -> a -> unit expr =
  fun r f -> fun x -> << match $r with Refl -> $(let _ = f x in << () >>) >>
;;
[%%expect{|
Line 2, characters 59-60:
2 |   fun r f -> fun x -> << match $r with Refl -> $(let _ = f x in << () >>) >>
                                                               ^
Error: The value "x" has type "a" but an expression was expected of type "b"
|}]

(* An expansion made at the level of the match is not used again
   below it. *)

let bad12 :
  type a b. (a, b) eql expr -> a expr -> (b -> unit) -> a -> unit expr =
  fun r c f x ->
    << match $r with Refl -> let _ : b = $c in $(let _ = f x in << () >>) >>
;;
[%%expect{|
Line 4, characters 59-60:
4 |     << match $r with Refl -> let _ : b = $c in $(let _ = f x in << () >>) >>
                                                               ^
Error: The value "x" has type "a" but an expression was expected of type "b"
|}]

(* A splice of unrelated code at the refined type is refused. *)

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

(* A code value is not converted at the level of the code that builds
   the quotation. *)

type _ ty = Int : int ty | Str : string ty | Bool : bool ty
;;
[%%expect{|
type _ ty = Int : int ty | Str : string ty | Bool : bool ty
|}]

let typecase_value :
  type a r. a ty expr -> (int expr -> r expr) -> a expr -> r expr =
  fun r fi x ->
    << match $r with Int -> $(fi x) | Str -> $(fi x) | Bool -> $(fi x) >>
;;
[%%expect{|
Line 4, characters 33-34:
4 |     << match $r with Int -> $(fi x) | Str -> $(fi x) | Bool -> $(fi x) >>
                                     ^
Error: The value "x" has type "a expr" but an expression was expected of type
         "int expr"
       Type "a" is not compatible with type "int"
|}]

(* An existential type does not escape the branch. *)

type _ pack = Pack : 'e * ('e -> int) -> unit pack
;;
[%%expect{|
type _ pack = Pack : 'e * ('e -> int) -> unit pack
|}]

let bad11 : unit pack expr -> int expr = fun p ->
  let cell = ref None in
  let _ : unit expr =
    << match $p with Pack (x, f) ->
         $(cell := Some (<< x >>, << f >>); << () >>) >> in
  match !cell with Some (c, g) -> << $g $c >> | None -> assert false
;;
[%%expect{|
Line 5, characters 28-29:
5 |          $(cell := Some (<< x >>, << f >>); << () >>) >> in
                                ^
Error: The value "x" has type "$e" but an expression was expected of type "'a"
       The type constructor "$e" would escape its scope
       Hint: "$e" is an existential type bound by the constructor "Pack".
|}]
