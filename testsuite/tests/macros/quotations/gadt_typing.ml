(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An equation that a match on a GADT learns is usable at the level of
   the match and above. *)

type (_, _) eql = Refl : ('a, 'a) eql

(* The match is in quoted code, and the equation is used on the result
   of a splice, which lands in the branch. *)
let ok2 : type a b. (a, b) eql expr -> (a expr -> a expr) -> (a -> b) expr =
  fun r f -> << fun x -> match $r with Refl -> $(f << x >>) >>

(* It is used in a quotation inside the splice, which is back at the
   level of the match. *)
let c17 : type a b. (a, b) eql expr -> (b expr -> b expr) -> (a -> b) expr =
  fun r f -> << fun x -> match $r with Refl -> $(f << (x : b) >>) >>

(* The match is at the level of the code that builds the quotation. *)
let ok8 : type a b. (a, b) eql -> a expr -> b expr =
  fun r c -> << $(match r with Refl -> c) >>
let ok9 : type a b. (a, b) eql -> a expr -> b expr =
  fun r c -> match r with Refl -> << $c >>

(* An expansion made inside the splice, through an equation learned
   there, is still valid in the quotation above it. *)
let ok13 : type a b. (a, b) eql -> a expr -> (b expr -> b expr) -> b expr =
  fun r c k -> << $(match r with Refl -> let _ : b expr = c in k c) >>

(* The pattern of a parameter learns equations as an arm does. *)
let param : type a b. (a, b) eql expr -> (b expr -> b expr) -> (a -> b) expr =
  fun r f -> << fun x -> (fun (Refl : (a, b) eql) -> $(f << (x : b) >>)) $r >>

(* A case analysis on a type in the generated program: the type of a
   code value is converted inside a quotation. *)
type _ ty = Int : int ty | Str : string ty | Bool : bool ty

let typecase :
  type a r. a ty expr -> (int expr -> r expr) -> (string expr -> r expr)
            -> (bool expr -> r expr) -> a expr -> r expr =
  fun r fi fs fb x ->
    << match $r with
       | Int -> $(fi << ($x : int) >>) | Str -> $(fs << ($x : string) >>)
       | Bool -> $(fb << ($x : bool) >>) >>

let typecase_gen :
  type a r. a ty expr -> (r expr -> int expr) -> (r expr -> string expr)
            -> (r expr -> bool expr) -> r expr -> a expr =
  fun r fi fs fb s ->
    << match $r with Int -> $(fi s) | Str -> $(fs s) | Bool -> $(fb s) >>

(* Code that uses an equation may leave the branch: through an
   exception, or a reference.  The type checker accepts it. *)
let bad7 : type a b. (a, b) eql expr -> (b -> unit) -> (a -> b) expr =
  fun r _ ->
    let exception E of b expr in
    << fun x -> $(try << match $r with Refl -> $(raise (E << x >>)) >>
                  with E y -> y) >>

let bad9 : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    Option.get !cell

(* An existential type of a match in quoted code *)
type _ pack = Pack : 'e * ('e -> int) -> unit pack

let ok10 : unit pack expr -> int expr = fun p ->
  << match $p with Pack (x, f) ->
       $(let c = << x >> and g = << f >> in << $g $c >>) >>
