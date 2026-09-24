(* TEST
 expect;
*)

(* GADT refinement across stages.  An equation learned by a match at
   one stage is available to later stages, never to earlier ones (a
   splice in the branch runs before the match has happened): that is
   the typing rule.  Code built inside such a splice may use the
   equation, and is not committed to landing in the branch; that
   escape is caught when the generator runs, through the evidence the
   arm binds (gadt_evidence.ml).  Each case is numbered as in
   GADTS.md. *)

type (_,_) eql = Refl : ('a, 'a) eql
;;

[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
|}]

(* ok1, ok4, ok6: stagingless. *)

let ok1 : type a b. (a, b) eql -> (b -> b) -> (a -> b) =
  fun r f -> fun x -> match r with Refl -> f x
let ok4 : type a b. (a, b) eql -> (b -> unit) -> a -> unit =
  fun r f -> fun x -> match r with Refl -> let _ = f x in ()
let ok6 : type a b. (a, b) eql -> (b -> unit) -> (a -> b) =
  fun r f ->
    let exception E of b in
    fun x -> try match r with Refl -> (raise (E x)) with E y -> y
;;

[%%expect{|
val ok1 : ('a, 'b) eql -> ('b -> 'b) -> 'a -> 'b = <fun>
val ok4 : ('a, 'b) eql -> ('b -> unit) -> 'a -> unit = <fun>
val ok6 : ('a, 'b) eql -> ('b -> unit) -> 'a -> 'b = <fun>
|}]

(* ok2: the match is in the quoted code, and the equation is used on
   the RESULT of the splice, which lands in the branch. *)

let ok2 : type a b. (a, b) eql expr -> (a expr -> a expr) -> (a -> b) expr =
  fun r f -> << fun x -> match $r with Refl -> $(f <<x>>) >>
;;

[%%expect{|
val ok2 : ('a, 'b) eql expr -> ('a expr -> 'a expr) -> ('a -> 'b) expr =
  <fun>
|}]

(* bad5, principle (1): f x runs inside the splice, before the match. *)

let bad5 : type a b. (a, b) eql expr -> (b -> unit) -> a -> unit expr =
  fun r f -> fun x -> << match $r with Refl -> $(let _ = f x in << () >>) >>
;;

[%%expect{|
Line 2, characters 59-60:
2 |   fun r f -> fun x -> << match $r with Refl -> $(let _ = f x in << () >>) >>
                                                               ^
Error: The value "x" has type "a" but an expression was expected of type "b"
|}]

(* bad7, principle (2): << x >> is built at the match's stage but inside
   the splice, and the exception carries it out of the branch.  It
   typechecks; the generator is stopped (gadt_evidence_c05.ml). *)

let bad7 : type a b. (a, b) eql expr -> (b -> unit) -> (a -> b) expr =
  fun r f ->
    let exception E of b expr in
    << fun x -> $(try << match $r with Refl -> $(raise (E << x >>)) >>
                  with E y -> y) >>
;;

[%%expect{|
val bad7 : ('a, 'b) eql expr -> ('b -> unit) -> ('a -> 'b) expr = <fun>
|}]

(* ok8: the match in a splice body, at the generator's stage. *)

let ok8 : type a b. (a, b) eql -> a expr -> b expr =
  fun r c -> << $(match r with Refl -> c) >>
;;

[%%expect{|
val ok8 : ('a, 'b) eql -> 'a expr -> 'b expr = <fun>
|}]

(* ok9: the match at the generator's stage, the equation used inside a
   quotation in the branch. *)

let ok9 : type a b. (a, b) eql -> a expr -> b expr =
  fun r c -> match r with Refl -> << $c >>
;;

[%%expect{|
val ok9 : ('a, 'b) eql -> 'a expr -> 'b expr = <fun>
|}]

(* bad9, principle (2): the nested quotation is back at the match's
   level and may use the equation; what it builds is stashed and used
   where no Refl was ever evaluated.  It typechecks; the generator is
   stopped (gadt_evidence_c07.ml). *)

let bad9 : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    Option.get !cell
;;

[%%expect{|
val bad9 : ('a, 'b) eql expr -> 'a expr -> 'b expr = <fun>
|}]

(* bad12, the expansion-memo hazard: the equation is used at the
   match's level first -- let _ : b = $c, which expands a to b and
   memoises that on the type node -- and then needed inside the splice,
   below its level.  The memoised expansion must not be reused there:
   Ctype flushes the memo whenever the environment's level changes. *)

let bad12 : type a b. (a, b) eql expr -> a expr -> (b -> unit) -> a -> unit expr =
  fun r c f x ->
    << match $r with Refl -> let _ : b = $c in $(let _ = f x in << () >>) >>
;;

[%%expect{|
Line 3, characters 59-60:
3 |     << match $r with Refl -> let _ : b = $c in $(let _ = f x in << () >>) >>
                                                               ^
Error: The value "x" has type "a" but an expression was expected of type "b"
|}]

(* ok13, the memo in the sound direction: an expansion made inside the
   splice at the generator's level (a equation learned there) is
   still valid in the quotation above it. *)

let ok13 : type a b. (a, b) eql -> a expr -> (b expr -> b expr) -> b expr =
  fun r c k -> << $(match r with Refl -> let _ : b expr = c in k c) >>
;;

[%%expect{|
val ok13 : ('a, 'b) eql -> 'a expr -> ('b expr -> 'b expr) -> 'b expr = <fun>
|}]

(* ok10, bad11: the existential analogues, unchanged by the lift. *)

type _ pack = Pack : 'e * ('e -> int) -> unit pack

let ok10 : unit pack expr -> int expr = fun p ->
  << match $p with Pack (x, f) ->
       $(let c = << x >> and g = << f >> in << $g $c >>) >>
;;

[%%expect{|
type _ pack = Pack : 'e * ('e -> int) -> unit pack
val ok10 : unit pack expr -> int expr = <fun>
|}]

let bad11 : unit pack expr -> int expr = fun p ->
  let cell = ref None in
  let _ : unit expr =
    << match $p with Pack (x, f) -> $(cell := Some (<< x >>, << f >>); << () >>) >> in
  match !cell with Some (c, g) -> << $g $c >> | None -> assert false
;;

[%%expect{|
Line 4, characters 55-56:
4 |     << match $p with Pack (x, f) -> $(cell := Some (<< x >>, << f >>); << () >>) >> in
                                                           ^
Error: The value "x" has type "$e" but an expression was expected of type "'a"
       The type constructor "$e" would escape its scope
       Hint: "$e" is an existential type bound by the constructor "Pack".
|}]
