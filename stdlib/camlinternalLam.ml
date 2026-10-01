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

(* The types of this module mirror those of the compiler: [Ident.t] is
   that of typing/ident.ml, and the tags and records below are those of
   lambda/lambda.mli.  testsuite/tests/compiler-libs/test_quote_layout.ml
   checks the correspondence. *)

module Ident =
struct
  module Unscoped = struct
    type desc = { name: string; stamp: int }
    type state =
      | Udesc of desc
      | Ulink of t
    and t = { mutable state : state }

    let rec get_desc us =
      match us.state with
        Udesc d -> d
      | Ulink u -> get_desc u
  end

  type t =
      Local of { name: string; stamp: int }
    | Scoped of { name: string; stamp: int; scope: int }
    | Global of string
    | Predef of { name: string; stamp: int }
    | Unscoped of Unscoped.t

  let compare x y =
    match x, y with
    | Local x, Local y ->
        let c = x.stamp - y.stamp in
        if c <> 0 then c
        else compare x.name y.name
    | Local _, _ -> 1
    | _, Local _ -> (-1)
    | Scoped x, Scoped y ->
        let c = x.stamp - y.stamp in
        if c <> 0 then c
        else compare x.name y.name
    | Scoped _, _ -> 1
    | _, Scoped _ -> (-1)
    | Global x, Global y -> compare x y
    | Global _, _ -> 1
    | _, Global _ -> (-1)
    | Predef { stamp = s1; _ }, Predef { stamp = s2; _ } -> compare s1 s2
    | Predef _, _ -> 1
    | _, Predef _ -> (-1)
    | Unscoped x, Unscoped y ->
        let Unscoped.{ name = n1; stamp = s1 } = Unscoped.get_desc x in
        let Unscoped.{ name = n2; stamp = s2 } = Unscoped.get_desc y in
        let c = s1 - s2 in
        if c <> 0 then c
        else compare n1 n2
end

type structured_constant =
    Const_int of int
  | Const_char of char
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string
[@@warning "-37"]

type lam = Obj.t

(* The layout of [lambda] that the builders below rely on, and that
   test_quote_layout checks: the tag of each constructor built, and the
   value of each constant constructor stored. *)
let lvar_tag = 0
let lconst_tag = 2
let lfunction_tag = 4
let lletrec_tag = 7
let lprim_tag = 8
let lswitch_tag = 9
let lstringswitch_tag = 10
let lstaticraise_tag = 11
let lstaticcatch_tag = 12
let lsequence_tag = 15
let pmakearray_tag = 19  (* among the constructors of [primitive] *)
let pgenarray = 0        (* [array_kind] *)
let immutable = 0        (* [mutable_flag] *)
let loc_unknown = 0      (* [scoped_location] *)

let lconst (c : structured_constant) : lam =
  let b = Obj.new_block lconst_tag 1 in
  Obj.set_field b 0 (Obj.repr c);
  b

let int32_ops = Obj.raw_field (Obj.repr 0l) 0
let int64_ops = Obj.raw_field (Obj.repr 0L) 0
let nativeint_ops = Obj.raw_field (Obj.repr 0n) 0

let boxed_number_kind o =
  let ops = Obj.raw_field o 0 in
  if Nativeint.equal ops int32_ops then `Int32
  else if Nativeint.equal ops int64_ops then `Int64
  else if Nativeint.equal ops nativeint_ops then `Nativeint
  else `Other

let var (id : Ident.t) : lam =
  let b = Obj.new_block lvar_tag 1 in
  Obj.set_field b 0 (Obj.repr id);
  b

type rec_binding = { id : Ident.t; def : Obj.t }
[@@warning "-69"]

let is_function (l : lam) =
  let o = Obj.repr l in
  Obj.is_block o && Obj.tag o = lfunction_tag && Obj.size o = 1

let letrec (bindings : (Ident.t * lam) list) (body : lam) : lam =
  match bindings with
  | [] -> body
  | _ ->
      let binding (id, rhs) =
        if not (is_function rhs) then
          invalid_arg
            "CamlinternalLam.letrec: right-hand side is not a syntactic \
             function";
        { id; def = Obj.field (Obj.repr rhs) 0 }
      in
      let b = Obj.new_block lletrec_tag 2 in
      Obj.set_field b 0 (Obj.repr (List.map binding bindings));
      Obj.set_field b 1 body;
      b

let make_iarray (elems : lam list) : lam =
  match elems with
  | [] ->
      lconst (Const_block (0, []))
  | _ ->
      let prim = Obj.new_block pmakearray_tag 2 in
      Obj.set_field prim 0 (Obj.repr pgenarray);
      Obj.set_field prim 1 (Obj.repr immutable);
      let b = Obj.new_block lprim_tag 3 in
      Obj.set_field b 0 prim;
      Obj.set_field b 1 (Obj.repr elems);
      Obj.set_field b 2 (Obj.repr loc_unknown);
      b

(* The size of a jump table is bounded by bytecode, whose SWITCH
   instruction gives it in 16 bits (bytecomp/emitcode.ml). *)
let max_table = 65535

let sequence (a : lam) (b : lam) : lam =
  let s = Obj.new_block lsequence_tag 2 in
  Obj.set_field s 0 a;
  Obj.set_field s 1 b;
  s

type lambda_switch =
  { sw_numconsts : int;
    sw_consts : (int * lam) list;
    sw_numblocks : int;
    sw_blocks : (int * lam) list;
    sw_failaction : lam option }
[@@warning "-69"]

let switch (arg : lam) ~numconsts (cases : (int * lam) list)
    (fail : lam option) : lam =
  if numconsts <= 0 then
    invalid_arg "CamlinternalLam.switch: numconsts is not positive";
  if numconsts > max_table then
    invalid_arg "CamlinternalLam.switch: numconsts exceeds 65535";
  let cases = List.stable_sort (fun (i, _) (j, _) -> compare i j) cases in
  let rec check previous = function
    | [] -> ()
    | (i, _) :: rest ->
        if i < 0 || i >= numconsts then
          invalid_arg "CamlinternalLam.switch: case out of range";
        if i = previous then
          invalid_arg "CamlinternalLam.switch: duplicate case";
        check i rest
  in
  check (-1) cases;
  if Option.is_none fail && List.length cases < numconsts then
    invalid_arg
      "CamlinternalLam.switch: incomplete cases and no failure action";
  (* A switch with a case for every integer has no failure action. *)
  let fail = if List.length cases = numconsts then None else fail in
  match cases, fail with
  | [], Some fail -> sequence arg fail
  | _ ->
      let sw =
        { sw_numconsts = numconsts; sw_consts = cases;
          sw_numblocks = 0; sw_blocks = []; sw_failaction = fail }
      in
      let b = Obj.new_block lswitch_tag 3 in
      Obj.set_field b 0 arg;
      Obj.set_field b 1 (Obj.repr sw);
      Obj.set_field b 2 (Obj.repr loc_unknown);
      b

(* Negative exit numbers do not coincide with the compiler's. *)
let exit_count = ref 0

let fresh_exit () =
  decr exit_count;
  !exit_count

let exit_to k : lam =
  let b = Obj.new_block lstaticraise_tag 2 in
  Obj.set_field b 0 (Obj.repr k);
  Obj.set_field b 1 (Obj.repr []);
  b

let catch (body : lam) k (handler : lam) : lam =
  let b = Obj.new_block lstaticcatch_tag 3 in
  Obj.set_field b 0 body;
  Obj.set_field b 1 (Obj.repr (k, []));
  Obj.set_field b 2 handler;
  b

(* Give each body one occurrence: keys with equal bodies share it. *)
let share (groups : ('k list * lam) list) (fail : lam option)
  : ('k * lam) list * (lam -> lam) =
  let groups =
    match fail with
    | None -> groups
    | Some f -> List.filter (fun (_, body) -> compare body f <> 0) groups
  in
  let rec merge = function
    | [] -> []
    | (keys, body) :: rest ->
        let same, others =
          List.partition (fun (_, b) -> compare b body = 0) rest in
        (List.concat (keys :: List.map fst same), body) :: merge others
  in
  List.fold_left
    (fun (cases, handled) (keys, body) ->
       match keys with
       | [ k ] -> ((k, body) :: cases, handled)
       | ks ->
           let e = fresh_exit () in
           (List.map (fun k -> (k, exit_to e)) ks @ cases,
            fun sw -> catch (handled sw) e body))
    ([], Fun.id) (merge groups)

let switch_intervals (arg : lam) ~numconsts
    (groups : ((int * int) list * lam) list) (fail : lam option) : lam =
  let invalid what = invalid_arg ("CamlinternalLam.switch_intervals: " ^ what) in
  if numconsts <= 0 then invalid "numconsts is not positive";
  if numconsts > max_table then invalid "numconsts exceeds 65535";
  List.iter
    (fun (intervals, _) ->
       if intervals = [] then invalid "group without intervals";
       List.iter
         (fun (lo, hi) ->
            if lo > hi then invalid "empty interval";
            if lo < 0 || hi >= numconsts then invalid "interval out of range")
         intervals)
    groups;
  let rec covering next = function
    | [] -> next = numconsts
    | (lo, hi) :: rest ->
        if lo < next then invalid "overlapping intervals";
        let contiguous = lo = next in
        covering (hi + 1) rest && contiguous
  in
  let complete =
    covering 0 (List.sort compare (List.concat_map fst groups)) in
  if Option.is_none fail && not complete then
    invalid "incomplete intervals and no failure action";
  let integers intervals =
    List.concat_map
      (fun (lo, hi) -> List.init (hi - lo + 1) (fun i -> lo + i)) intervals
  in
  let cases, handled =
    share
      (List.map (fun (intervals, body) -> (integers intervals, body)) groups)
      fail
  in
  handled (switch arg ~numconsts cases fail)

let string_switch (arg : lam) (cases : (string * lam) list) (default : lam)
  : lam =
  let cases =
    List.stable_sort (fun (s, _) (t, _) -> String.compare s t) cases in
  let rec check = function
    | (s, _) :: ((t, _) :: _ as rest) ->
        if String.equal s t then
          invalid_arg "CamlinternalLam.string_switch: duplicate case";
        check rest
    | [ _ ] | [] -> ()
  in
  check cases;
  let cases, handled =
    share (List.map (fun (s, body) -> ([ s ], body)) cases) (Some default)
  in
  match List.stable_sort (fun (s, _) (t, _) -> String.compare s t) cases with
  | [] -> sequence arg default
  | cases ->
      let b = Obj.new_block lstringswitch_tag 4 in
      Obj.set_field b 0 arg;
      Obj.set_field b 1 (Obj.repr cases);
      Obj.set_field b 2 (Obj.repr (Some default));
      Obj.set_field b 3 (Obj.repr loc_unknown);
      handled b

let const : 'a. 'a -> lam =
  let rec constant_of : 'a. 'a -> structured_constant = fun o ->
    let o = Obj.repr o in
    if Obj.is_int o then Const_int (Obj.obj o)
    (* Strings and boxed numbers are copied, so that the constant does
       not inherit their sharing. *)
    else if Obj.tag o = Obj.string_tag
    then Const_immstring (Obj.obj (Obj.dup o))
    else if Obj.tag o = Obj.custom_tag then begin
      match boxed_number_kind o with
      | `Int32 -> Const_int32 (Obj.obj (Obj.dup o))
      | `Int64 -> Const_int64 (Obj.obj (Obj.dup o))
      | `Nativeint -> Const_nativeint (Obj.obj (Obj.dup o))
      | `Other ->
          invalid_arg "CamlinternalLam.const: unsupported custom block"
    end
    else if Obj.tag o = Obj.double_tag
    then Const_float (Printf.sprintf "%h" (Obj.obj o : float))
    else if Obj.tag o = Obj.double_array_tag
    then
      Const_float_array
        (List.init (Array.length (Obj.obj o : float array))
           (fun i -> Printf.sprintf "%h" (Obj.double_field o i)))
    else if Obj.is_block o
            && Obj.tag o <= Obj.last_non_constant_constructor_tag
    then Const_block (Obj.tag o,
                      List.init (Obj.size o)
                                (fun i -> constant_of (Obj.field o i)))
    else
      invalid_arg
        (Printf.sprintf "CamlinternalLam.const: unsupported tag %d"
           (Obj.tag o))
  in
  fun x -> lconst (constant_of x)
