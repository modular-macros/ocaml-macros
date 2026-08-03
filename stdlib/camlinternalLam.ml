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

let lconst_tag = 2

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

let lvar_tag = 0
let lfunction_tag = 4
let lletrec_tag = 7

let var (id : Ident.t) : lam =
  let b = Obj.new_block lvar_tag 1 in
  Obj.set_field b 0 (Obj.repr id);
  b

type rec_binding = { id : Ident.t; def : Obj.t }
[@@warning "-69"]

let letrec (bindings : (Ident.t * lam) list) (body : lam) : lam =
  match bindings with
  | [] -> body
  | _ ->
      let binding (id, rhs) =
        let o = Obj.repr rhs in
        if not (Obj.is_block o && Obj.tag o = lfunction_tag
                && Obj.size o = 1)
        then
          invalid_arg
            "CamlinternalLam.letrec: right-hand side is not a syntactic \
             function";
        { id; def = Obj.field o 0 }
      in
      let b = Obj.new_block lletrec_tag 2 in
      Obj.set_field b 0 (Obj.repr (List.map binding bindings));
      Obj.set_field b 1 body;
      b

let lprim_tag = 8
let pmakearray_tag = 19

let make_iarray (elems : lam list) : lam =
  match elems with
  | [] ->
      lconst (Const_block (0, []))
  | _ ->
      let prim = Obj.new_block pmakearray_tag 2 in
      Obj.set_field prim 0 (Obj.repr 0 );
      Obj.set_field prim 1 (Obj.repr 0 );
      let b = Obj.new_block lprim_tag 3 in
      Obj.set_field b 0 prim;
      Obj.set_field b 1 (Obj.repr elems);
      Obj.set_field b 2 (Obj.repr 0 );
      b

let const : 'a. 'a -> lam =
  let rec constant_of : 'a. 'a -> structured_constant = fun o ->
    let o = Obj.repr o in
    if Obj.is_int o then Const_int (Obj.obj o)
    else if Obj.tag o = Obj.string_tag
    then Const_immstring (Obj.obj o)
    else if Obj.tag o = Obj.custom_tag then begin
      match boxed_number_kind o with
      | `Int32 -> Const_int32 (Obj.obj o)
      | `Int64 -> Const_int64 (Obj.obj o)
      | `Nativeint -> Const_nativeint (Obj.obj o)
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
