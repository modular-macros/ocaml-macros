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

(* This module is compiled without the initial open of Stdlib, so that
   its quotations can name the runtime's own exceptions: Stdlib rebinds
   them, and a constructor of a unit is one of run-time code, which a
   quotation's code, made for another program, cannot mention. *)

let no_case_of_switch () = << Stdlib.raise (Invalid_argument "Expr.switch") >>

let no_case_of_switch_intervals () =
  << Stdlib.raise (Invalid_argument "Expr.switch_intervals") >>

open Stdlib

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

(* The builders below make terms that no quotation of fixed shape can
   express, through CamlinternalLam, and keep what a quotation keeps:
   the free variables of the code they combine, and fresh binders. *)

let quote (lam, fvs) : 'a expr = Obj.magic (lam, fvs)

(* The term of [e], its free variables added to [fvs] *)
let term fvs (e : 'a expr) =
  let (lam, f) = unquote e in
  fvs := CamlinternalQuote.Identifier.merge_free_vars f !fvs;
  lam

(* The errors of a builder of CamlinternalLam, reported as [name]'s *)
let reporting name f =
  try f () with Invalid_argument msg ->
    match String.index_opt msg ':' with
    | Some i -> invalid_arg (name ^ String.sub msg i (String.length msg - i))
    | None -> invalid_arg name

module type immediate = sig type t [@@immediate] end

(* A switch on an immediate value is checked: its jump table is indexed
   by the integer that represents the value, which must be in
   [0, numconsts).  A value outside that range takes the default, as a
   value without a case does; both reach it through one static exit,
   so that its code is built once.  [dispatch] builds the switch on the
   term of the scrutinee's integer, given the term of that exit. *)
let checked name ~numconsts (arg : 'k expr) (default : 'a expr)
    (dispatch : CamlinternalQuote.Identifier.set_of_t ref ->
     CamlinternalLam.lam -> CamlinternalLam.lam -> CamlinternalLam.lam)
  : 'a expr =
  if numconsts <= 0 then invalid_arg (name ^ ": numconsts is not positive");
  let k = CamlinternalLam.fresh_exit () in
  let leave : 'a expr =
    quote (CamlinternalLam.exit_to k, CamlinternalQuote.Identifier.empty) in
  let body =
    << let i : int = Obj.magic $arg in
       if i >= 0 && i < $(int numconsts) then
         $(let fvs = ref CamlinternalQuote.Identifier.empty in
           let i = term fvs << i >> in
           let sw = dispatch fvs i (CamlinternalLam.exit_to k) in
           quote (sw, !fvs))
       else $leave >>
  in
  let fvs = ref CamlinternalQuote.Identifier.empty in
  let body = term fvs body in
  let default = term fvs default in
  quote (CamlinternalLam.catch body k default, !fvs)

let switch (module I : immediate) (arg : I.t expr) ~numconsts
    (cases : (I.t * 'a expr) list) (default : 'a expr option) : 'a expr =
  let default =
    match default with
    | Some d -> d
    | None -> no_case_of_switch ()
  in
  checked "Expr.switch" ~numconsts arg default (fun fvs i leave ->
      let cases =
        List.map (fun (key, e) -> ((Obj.magic key : int), term fvs e)) cases
      in
      reporting "Expr.switch" (fun () ->
          CamlinternalLam.switch i ~numconsts cases (Some leave)))

let switch_intervals (module I : immediate) (arg : I.t expr) ~numconsts
    (groups : ((I.t * I.t) list * 'a expr) list) (default : 'a expr option)
  : 'a expr =
  let default =
    match default with
    | Some d -> d
    | None -> no_case_of_switch_intervals ()
  in
  checked "Expr.switch_intervals" ~numconsts arg default (fun fvs i leave ->
      let interval (lo, hi) = ((Obj.magic lo : int), (Obj.magic hi : int)) in
      let groups =
        List.map (fun (ivs, e) -> (List.map interval ivs, term fvs e)) groups
      in
      reporting "Expr.switch_intervals" (fun () ->
          CamlinternalLam.switch_intervals i ~numconsts groups (Some leave)))

let string_switch (arg : string expr) (cases : (string * 'a expr) list)
    (default : 'a expr) : 'a expr =
  let fvs = ref CamlinternalQuote.Identifier.empty in
  let arg = term fvs arg in
  let cases = List.map (fun (s, e) -> (s, term fvs e)) cases in
  let default = term fvs default in
  reporting "Expr.string_switch" (fun () ->
      quote (CamlinternalLam.string_switch arg cases default, !fvs))

module Letrec = struct
  (* An identifier of the quotations' scopes is a term's identifier. *)
  let ident (id : CamlinternalQuote.Identifier.t) : CamlinternalLam.Ident.t =
    Obj.magic id

  type binding = {
    id : CamlinternalQuote.Identifier.t;
    mutable definition : CamlinternalLam.lam option;
  }

  type group = {
    mutable bindings : binding list;  (* the latest first *)
    mutable free : CamlinternalQuote.Identifier.set_of_t;
  }

  (* A binder is made by the handler of its group, which resumes the
     rest of the group in the binder's scope.  The handler's value case
     runs there too, so the term it builds leaves every binder's scope
     in turn, each removing its binder from the free variables and
     checking the others, as a quotation's binders do. *)
  type _ Effect.t += Bind : group -> binding Effect.t

  let group (body : group -> 'a expr) : 'a expr =
    let g = { bindings = []; free = CamlinternalQuote.Identifier.empty } in
    let result =
      match unquote (body g) with
      | (lam, fvs) ->
          let definition b =
            match b.definition with
            | Some d -> (ident b.id, d)
            | None -> invalid_arg "Expr.Letrec.group: a binding is undefined"
          in
          (CamlinternalLam.letrec (List.rev_map definition g.bindings) lam,
           CamlinternalQuote.Identifier.merge_free_vars g.free fvs)
      | effect Bind g', k when g' == g ->
          let b =
            { id = CamlinternalQuote.Identifier.fresh "f"; definition = None }
          in
          g.bindings <- b :: g.bindings;
          CamlinternalQuote.Identifier.new_scope [ b.id ]
            (fun () -> Effect.Deep.continue k b)
    in
    quote result

  let bind (g : group) (definition : 'a expr -> 'a expr) : 'a expr =
    let b =
      match Effect.perform (Bind g) with
      | b -> b
      | exception Effect.Unhandled (Bind _) ->
          invalid_arg "Expr.Letrec.bind: outside its group"
    in
    let use =
      quote (CamlinternalLam.var (ident b.id),
             CamlinternalQuote.Identifier.free_var b.id)
    in
    (* The definition may bind in the group too: its free variables are
       added to the group's once it is built. *)
    let fvs = ref CamlinternalQuote.Identifier.empty in
    let d = term fvs (definition use) in
    if not (CamlinternalLam.is_function d) then
      invalid_arg "Expr.Letrec.bind: the definition is not a function";
    g.free <- CamlinternalQuote.Identifier.merge_free_vars !fvs g.free;
    b.definition <- Some d;
    use
end
