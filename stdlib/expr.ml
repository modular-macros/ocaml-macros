(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type +!'a t = 'a expr

(* A code value is a pair of a term and the set of its free variables.
   A lifted value is a constant, which has none. *)
let lift : 'a. 'a -> 'a expr = fun v ->
  Obj.magic (CamlinternalLam.const v, CamlinternalQuote.Identifier.empty)

(* The same pair, read back from an expression built elsewhere. *)
let unquote (e : 'a expr)
  : CamlinternalLam.lam * CamlinternalQuote.Identifier.set_of_t =
  Obj.magic e

let unit () = << () >>

let bool = function
  | false -> << false >>
  | true  -> << true  >>

let char (c : char) : char expr = lift c

let uchar (u : Uchar.t) : Uchar.t expr = lift u

let int (n : int) : int expr = lift n

let int32 (n : int32) : int32 expr = lift n

let int64 (n : int64) : int64 expr = lift n

let nativeint (n : nativeint) : nativeint expr = lift n

let float (f : float) : float expr = lift f

let string (s : string) : string expr = lift s

(* A [Complex.t] is a flat float record, covered by the constant
   classification's float-block case. *)
let complex (z : Complex.t) : Complex.t expr = lift z

let option = function
  | None   -> << None    >>
  | Some e -> << Some $e >>

(* Not tail-recursive (nor is the quotation nesting it builds): stack
   use is proportional to the list length, as for [List.map]. *)
let rec list = function
  | [] -> << [] >>
  | x :: xs -> << $x :: $(list xs) >>

(* No quotation builds an n-ary node, so the immutable array is
   constructed directly as a term over the element terms, its
   free-variable set the union of theirs.  [parts] is not
   tail-recursive: stack use is proportional to [n], as for
   [Array.init]. *)
let iarray (n : int) (f : int -> 'a expr) : 'a iarray expr =
  if n < 0 then invalid_arg "Expr.iarray";
  let rec parts i =
    if i = n then ([], CamlinternalQuote.Identifier.empty)
    else begin
      let (lam, fvs) = unquote (f i) in
      let (lams, rest) = parts (i + 1) in
      (lam :: lams, CamlinternalQuote.Identifier.merge_free_vars fvs rest)
    end
  in
  let (lams, fvs) = parts 0 in
  Obj.magic (CamlinternalLam.make_iarray lams, fvs)

let either = function
  | Either.Left  l -> << Either.Left  $l >>
  | Either.Right r -> << Either.Right $r >>

let result = function
  | Result.Ok    l -> << Result.Ok    $l >>
  | Result.Error r -> << Result.Error $r >>

let pair (fst, snd) = << ($fst, $snd) >>

let func f = << fun x -> $(f << x >>) >>

let unsafe_lift_constant (v : 'a) : 'a expr = lift v
