open CamlinternalAST

(* ------------------------------------------------------------------------ *)
(* Monomorphic versions of the types from CamlinernalAST, using abstract
   types for the type parameters. *)

type abstract_attribute
type abstract_core_type
type abstract_module_expr
type abstract_class_structure

type expression =
  (abstract_attribute,
   abstract_core_type,
   abstract_module_expr,
   abstract_class_structure) CamlinternalAST.expression

type expression_desc =
  (abstract_attribute,
   abstract_core_type,
   abstract_module_expr,
   abstract_class_structure) CamlinternalAST.expression_desc

type pattern =
  (abstract_attribute,
   abstract_core_type,
   abstract_module_expr,
   abstract_class_structure) CamlinternalAST.pattern

type pattern_desc =
  (abstract_attribute,
   abstract_core_type,
   abstract_module_expr,
   abstract_class_structure) CamlinternalAST.pattern_desc

type case =
  (abstract_attribute,
   abstract_core_type,
   abstract_module_expr,
   abstract_class_structure) CamlinternalAST.case

(* ------------------------------------------------------------------------ *)
(* Accumulating maps over common structures *)

let accum_option f loc acc o =
  match o with
  | None -> (acc, None)
  | Some x ->
      let acc, x = f loc acc x in
        (acc, Some x)

let rec accum_list f loc acc l =
  match l with
  | [] -> (acc, [])
  | x :: rest ->
      let acc, x = f loc acc x in
      let acc, rest = accum_list f loc acc rest in
        (acc, x :: rest)

let rec accum_alist f loc acc l =
  match l with
  | [] -> (acc, [])
  | (k, x) :: rest ->
      let acc, x = f loc acc x in
      let acc, rest = accum_alist f loc acc rest in
        (acc, (k, x) :: rest)

(* ------------------------------------------------------------------------ *)
(* Stack marks, a simple form of dynamic binding *)
module Stackmark : sig

  type t

  val check : t -> bool

  val region : (t -> 'a) -> 'a

end = struct

  (* A robust and truly minimalistic implementation of stack-marks.
     A stack-mark is created by 'with_stack_mark' function. Since
     the only operation on a stackmark is to test if it is valid,
     the stackmark is realized as a thunk unit -> bool.
  *)
  type t = unit -> bool           (* true if valid *)

  let check t = t ()

  (* The simple implementation of stackmark_region_fn, appropriate
     when no delimited control is used.
     The mark is a ref bool cell, containing true within
     stackmark_region_fn's dynamic region.
  *)
  let region body =
    let mark = ref true in
      try
        let r = body (fun () -> !mark) in
        mark := false;                      (* invalidate the mark *)
        r
      with e -> mark := false; raise e

end

(* ------------------------------------------------------------------------ *)
(* Simple heap *)
(* A mapping of keys to values. Priority is used for the sake of
   efficient operations. Also, values with the same priority are
   considered equivalent (belong to the same binding region)
   and are collapsed, lazily.

   The invariant: for each non-leaf
   node, the priority of the node is strictly greater than the priorities
   of any of the child nodes. The order of priorities between
   the children can be arbitrary.
*)

module Priority : sig

  type t

  val fresh : unit -> t

end = struct

  type t = int

  let counter = ref 0

  (* Keep in mind the invariant that variables of the same priority
     comes from the same binding location. So, we must keep the
     priorities unique to binders. Giving binders monotonically
     increasing priorities is helpful: the innermost binding
     has the highest priority and it will be at the top of the heap,
     the easiest to remove.
       *)
  let fresh () =
    incr counter;
    !counter

end

module Heap : sig

  type 'a t

  val empty : 'a t

  val singleton : Priority.t -> 'a -> 'a t

  val merge : 'a t -> 'a t -> 'a t

  val remove : 'a t -> Priority.t -> 'a t

  val choose : 'a t -> 'a option

  val iter : ('a -> unit) -> 'a t -> unit

end = struct

  type 'a t =
    | Nil
    | HNode of Priority.t * 'a * 'a t * 'a t

  let empty = Nil

  let singleton prio v =
    HNode (prio, v, Nil, Nil)

  let rec merge h1 h2 =
    match h1, h2 with
    | Nil, h | h, Nil-> h
    | HNode (p1, v1, l1, r1), HNode (p2, v2, l2, r2) -> begin
        match compare p1 p2 with
        | 0 -> HNode (p1, v1, merge l1 l2, merge r1 r2) (* same keys *)
        | n when n < 0 -> HNode (p2, v2, merge h1 l2, r2)
        | _ -> HNode (p1, v1, l1, merge h2 r1)
      end

  let rec remove h p =
    match h with
    | Nil -> Nil
    | HNode (pn, v, h1, h2) -> begin
        match compare p pn with
        | 0 -> merge h1 h2              (* p cannot occur in h1 or h2 *)
        | n when n > 0 -> h             (* entire tree has the lower priority *)
        | _ -> HNode (pn, v, remove h1 p, remove h2 p)
      end

  let choose = function
    | Nil -> None
    | HNode (_, v, _, _) -> Some v

  let rec iter f = function
    | Nil -> ()
    | HNode (_, v, h1, h2) -> begin
        f v;
        iter f h1;
        iter f h2
      end

end

(* ------------------------------------------------------------------------ *)
(* Simple map *)

module Symbol : sig

  type t

  val fresh : unit -> t

  val to_string : t -> string

  val compare : t -> t -> int

end = struct

  type t = int

  let counter = ref 0

  let fresh () =
    incr counter;
    !counter

  let to_string = string_of_int

  let compare (x : t) (y : t) = compare x y

end

module Map : sig

  type 'a t

  val empty : 'a t

  val singleton : Symbol.t -> 'a -> 'a t

  val add : Symbol.t -> 'a -> 'a t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit

  val merge : ('a -> 'a -> unit) -> 'a t -> 'a t -> 'a t

  val choose : 'a t -> 'a option

  val same : ('a -> unit) -> 'a t -> ('b -> unit) -> 'b t -> unit

end = struct

  module SymbolMap = Map.Make(Symbol)

  type 'a t = 'a SymbolMap.t

  let empty = SymbolMap.empty

  let singleton = SymbolMap.singleton

  let add = SymbolMap.add

  let iter f t =
    SymbolMap.iter (fun _ v -> f v) t

  let merge intersection t1 t2 =
    SymbolMap.merge
      (fun _ v1 v2 ->
        match v1, v2 with
        | None, None -> None
        | Some v, None -> Some v
        | None, Some v -> Some v
        | Some v1, Some v2  -> intersection v1 v2; None)
      t1 t2

  let choose t =
    match SymbolMap.choose t with
    | exception Not_found -> None
    | (_, v) -> Some v

  let same left t1 right t2 =
    ignore
      (SymbolMap.merge
         (fun _ v1 v2 ->
           match v1, v2 with
           | None, None -> None
           | Some v, None -> left v; None
           | None, Some v -> right v; None
           | Some _, Some _  -> None)
         t1 t2)

end

(* ------------------------------------------------------------------------ *)
(* Representation of locations *)

module Loc = struct

  type t = location

  let none =
    let loc =
      { Lexing.pos_fname = "_none_";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = -1; }
    in
      { loc_start = loc; loc_end = loc; loc_ghost = true }

  let unmarshal (s : string) : t =
    Marshal.from_string s 0

  let print ppf { loc_start; loc_end } =
    let open Lexing in
    if loc_start.pos_fname = "//toplevel//" then begin
      Format.fprintf ppf "Characters %i-%i"
        loc_start.pos_cnum loc_end.pos_cnum
    end else begin
      let startchar = loc_start.pos_cnum - loc_start.pos_bol in
      let endchar = loc_end.pos_cnum - loc_start.pos_cnum + startchar in
      Format.fprintf ppf "File \"%s\", line %i"
        loc_start.pos_fname loc_start.pos_lnum;
      if startchar >= 0 then
        Format.fprintf ppf ", characters %i-%i"
          startchar endchar
    end

end

(* ------------------------------------------------------------------------ *)
(* Representation of names *)

module Name = struct

  type t = string loc

  let mk txt loc = { txt; loc }

  let unmarshal s : t =
    Marshal.from_string s 0

end

(* ------------------------------------------------------------------------ *)
(* Representation of bound variables *)

module Var : sig

  type t

  val generate : Stackmark.t -> Priority.t -> string loc -> t

  val relocate : t -> Loc.t -> t

  val name : t -> string loc

  val txt : t -> string

  val loc : t -> Loc.t

  val symbol : t -> Symbol.t

  val heap : t -> t Heap.t

  val map : t -> t Map.t

  val valid : t -> bool

end = struct

  type t = {
    name : string loc;
    stackmark : Stackmark.t;
    symbol : Symbol.t;
    priority : Priority.t;
  }

  let generate stackmark priority name =
    let symbol = Symbol.fresh () in
    let txt = name.txt ^ "_" ^ (Symbol.to_string symbol) in
    let name = {name with txt} in
      { name; stackmark; symbol; priority }

  let relocate t loc =
    let name = { t.name with loc } in
      { t with name }

  let name t = t.name

  let txt t = t.name.txt

  let loc t = t.name.loc

  let symbol t = t.symbol

  let heap t = Heap.singleton t.priority t

  let map t = Map.singleton t.symbol t

  let valid t = Stackmark.check t.stackmark

end

(* ------------------------------------------------------------------------ *)
(* Representation of expressions *)

module ExpRepr : sig

  type t

  val mk : Loc.t -> Var.t Heap.t -> expression_desc -> t

  (* Verify the free variables of an expression and merge them into a
     heap. *)
  val merge :
    Loc.t -> Var.t Heap.t -> t -> Var.t Heap.t * expression

  module Closed : sig

    type t

  end

  val to_closed : t -> Closed.t

  val of_closed : Closed.t -> t

end  = struct

  type t =
    { heap : Var.t Heap.t;
      exp : expression; }

  let mk loc heap desc =
    let exp =
      { pexp_loc = loc;
        pexp_desc = desc;
        pexp_attributes = [] }
    in
      { heap; exp }

  (* This is a run-time error, rather than a translation-time error *)
  let scope_extrusion_error loc exp name =
    Format.fprintf Format.str_formatter
      "Scope extrusion detected at %a for expression built at %a \
       for the identifier %s bound at %a"
      Loc.print loc Loc.print exp.pexp_loc
      name.txt Loc.print name.loc;
    failwith (Format.flush_str_formatter ())

  (* Check to make sure that free variables in the potentially open
     code fragment are valid.
     If it weren't for delimited control, the order of stack marks is
     stable; therefore, if the maximal mark is valid then all
     smaller marks are valid as well.
     Delimited control spoils all that.
     When we capture some of the inner-bindings
     in a continuation and then reinstall that continuation at the
     top level, the `latest' free variable is valid but earlier are
     no longer valid:

    let r = ref ... in
    <<fun x1 x2 -> $(reset <<fun y1 y2 ->
                                $(shift k (r := k; k <<0>>))>>)>>
    .r <<2>>
    Here, y1 and y2 are valid but x1 and x2 are not.
  *)
  let merge loc acc { heap; exp } =
    Heap.iter
      (fun var ->
        if not (Var.valid var) then
          scope_extrusion_error loc exp (Var.name var))
      heap;
    (Heap.merge acc heap, exp)

  (* Check that the code is closed and return the closed code *)
  module Closed = struct

    (* The closed code is AST *)
    type t = expression

  end

  (* The same as close_code but return the closedness check as a thunk
     rather than performing it.
     This is useful for debugging and for showing the code
  *)
  let close_delay_check exp =
      match Heap.choose exp.heap with
      | None -> (exp.exp, fun () -> ())
      | Some var ->
        (exp.exp, fun () ->
          Format.fprintf Format.str_formatter
          "The code built at %a is not closed: \
           identifier %s bound at %a is free"
          Loc.print exp.exp.pexp_loc (Var.txt var)
          Loc.print (Var.loc var);
          failwith (Format.flush_str_formatter ()))

  let to_closed exp =
    let exp, check = close_delay_check exp in
    check (); exp

  let of_closed exp =
    let heap = Heap.empty in
      { heap; exp }

end

(* ------------------------------------------------------------------------ *)
(* Representation of patterns *)

module PatRepr : sig

  type t

  val mk : Loc.t -> Var.t Map.t -> pattern_desc -> t

  (* Validate the free variables of a pattern and merge them into a map
     whilst checking that they are disjoint (used for sub-patterns). *)
  val merge : Loc.t -> Var.t Map.t -> t -> Var.t Map.t * pattern

  (* Combine the binding variables of two patterns, checking that they
     are equivalent (used for or-patterns). *)
  val join :
    Loc.t -> t -> t -> Var.t Map.t * pattern * pattern

  (* Check a pattern binds no variables and return it. *)
  val nonbinding : Loc.t -> t -> pattern

  (* Check that a pattern binds exactly the given list of variables and
     return it. *)
  val check_bindings : Loc.t -> Var.t list -> t -> pattern

end = struct

  type t =
    { map : Var.t Map.t;
      pat : pattern; }

  let mk loc map desc =
    let pat =
      { ppat_loc = loc;
        ppat_desc = desc;
        ppat_attributes = [] }
    in
      { map; pat }

  let scope_extrusion_error loc pat name =
    Format.fprintf Format.str_formatter
      "Scope extrusion detected at %a for pattern built at %a \
       for the identifier %s"
      Loc.print loc Loc.print pat.ppat_loc name.txt;
    failwith (Format.flush_str_formatter ())

  let duplicate_binding_error loc pat var =
    Format.fprintf Format.str_formatter
       "Duplicate bindings detected at %a for pattern built at %a \
        for the identifier %s already bound at %a"
      Loc.print loc Loc.print pat.ppat_loc
      (Var.txt var) Loc.print (Var.loc var);
    failwith (Format.flush_str_formatter ())

  let merge loc acc { map; pat } =
    Map.iter
      (fun var ->
        if not (Var.valid var) then
          scope_extrusion_error loc pat (Var.name var))
      map;
    let duplicate var _ = duplicate_binding_error loc pat var in
    let map = Map.merge duplicate acc map in
      (map, pat)

  let mismatch_binding_error loc pat1 pat2 var =
    Format.fprintf Format.str_formatter
       "Mismatched bindings detected at %a for patterns built at %a and %a \
        for the identifier %s"
      Loc.print loc Loc.print pat1.ppat_loc
      Loc.print pat2.ppat_loc (Var.txt var);
    failwith (Format.flush_str_formatter ())

  let join loc {map = map1; pat = pat1} {map = map2; pat = pat2} =
    let diff var = mismatch_binding_error loc pat1 pat2 var in
    Map.same diff map1 diff map2;
    map1, pat1, pat2

  let unbound_var_error loc pat var =
    Format.fprintf Format.str_formatter
      "Pattern for binding at %a built at %a does not bind the identifier %s"
      Loc.print loc Loc.print pat.ppat_loc (Var.txt var);
    failwith (Format.flush_str_formatter ())

  let additional_var_error loc pat var =
    Format.fprintf Format.str_formatter
      "Pattern for binding at %a built at %a binds additional identifier %s"
      Loc.print loc Loc.print pat.ppat_loc (Var.txt var);
    failwith (Format.flush_str_formatter ())

  let nonbinding loc { map; pat } =
    match Map.choose map with
    | None -> pat
    | Some var -> additional_var_error loc pat var

  let check_bindings loc vars { map; pat } =
    let full_map =
      List.fold_left
        (fun acc var -> Map.add (Var.symbol var) var acc)
        Map.empty vars
    in
    let unbound var = unbound_var_error loc pat var in
    let additional var = additional_var_error loc pat var in
    Map.same unbound full_map additional map;
    pat

end

(* ------------------------------------------------------------------------ *)
(* Representation of cases *)

module CaseRepr : sig

  type t

  val mk :
    Var.t Heap.t -> pattern -> expression option -> expression -> t

  (* Combine the free variables of two cases. *)
  val merge : Loc.t -> Var.t Heap.t -> t -> Var.t Heap.t * case

end = struct

  type t =
    { heap : Var.t Heap.t;
      case : case; }

  let mk heap lhs guard rhs =
    let case =
      { pc_lhs = lhs;
        pc_guard = guard;
        pc_rhs = rhs; }
    in
      { heap; case }

  let merge _loc acc { heap; case } =
    let heap = Heap.merge acc heap in
      (heap, case)

end


(* ------------------------------------------------------------------------ *)

(* Bindings in the future stage *)
(* Recall, all bindings at the future stage are introduced by
   patterns, and hence are simple names, without any module qualifications.
*)

module Binding : sig

  val simple :
    Loc.t -> string loc -> (Var.t -> ExpRepr.t) ->
    Var.t Heap.t * pattern * expression

  val recursive :
    Loc.t -> string loc list ->
    (Var.t list -> ExpRepr.t list * ExpRepr.t) ->
    Var.t Heap.t * (pattern * expression) list * expression

  val pattern :
    Loc.t -> string loc list ->
    (Var.t list -> PatRepr.t * ExpRepr.t) ->
    Var.t Heap.t * pattern * expression

  val guarded :
    Loc.t -> string loc list ->
    (Var.t list -> PatRepr.t * ExpRepr.t * ExpRepr.t) ->
    Var.t Heap.t * pattern * expression * expression

end = struct

  let bind var =
    { ppat_loc = Var.loc var;
      ppat_desc = Ppat_var (Var.name var);
      ppat_attributes = []; }

  (* Generate a fresh name off the given name, enter a new binding region
     and evaluate a function passing it the generated name as exp_repr.
     Remove the generated name from the annotation on the resulting code_exp.
     Return that result and the generated name.
     This function embodies the translation of simple functions, for-loops,
     simple let-expressions, etc.
  *)
  let simple loc name f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let var = Var.generate mark prio name in
        let exp = f var in
        let heap, exp = ExpRepr.merge loc Heap.empty exp in
        let pat = bind var in
        (Heap.remove heap prio, pat, exp))

  let pair_binding_error loc npats nexps =
    Format.fprintf Format.str_formatter
      "Binding at %a has %d patterns but %d expressions"
      Loc.print loc npats nexps;
    failwith (Format.flush_str_formatter ())

  let pair_bindings loc pats exps =
    let npats = List.length pats in
    let nexps = List.length exps in
    if npats <> nexps then pair_binding_error loc npats nexps;
    List.combine pats exps

  let recursive loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (Var.generate mark prio) names in
        let defs, exp = f vars in
        let heap, defs = accum_list ExpRepr.merge loc Heap.empty defs in
        let heap, exp = ExpRepr.merge loc heap exp in
        let pats = List.map bind vars in
        let pairs = pair_bindings loc pats defs in
        (Heap.remove heap prio, pairs, exp))

  let pattern loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (Var.generate mark prio) names in
        let pat, exp = f vars in
        let pat = PatRepr.check_bindings loc vars pat in
        let heap, exp = ExpRepr.merge loc Heap.empty exp in
        (Heap.remove heap prio, pat, exp))

  let guarded loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (Var.generate mark prio) names in
        let pat, guard, exp = f vars in
        let pat = PatRepr.check_bindings loc vars pat in
        let heap, guard = ExpRepr.merge loc Heap.empty guard in
        let heap, exp = ExpRepr.merge loc heap exp in
        (Heap.remove heap prio, pat, guard, exp))

end

(* ------------------------------------------------------------------------ *)
(* Building Parsetree nodes *)

module Constant = struct

  type t = constant

  let unmarshal (s : string) : t =
    Marshal.from_string s 0

end

module Ident = struct

  type t = CamlinternalAST.lid CamlinternalAST.loc

  let unmarshal (s : string) : t =
    Marshal.from_string s 0

  let of_var var =
    let name = Var.name var in
    { name with txt = CamlinternalAST.Lident name.txt }

end

module Label = struct

  type t = string

  let none = ""

  let of_string (s : string) : t = s

end

module Variant = struct

  type t = string

  let of_string (s : string) : t = s

end

module Method = struct

  type t = string

  let of_string (s : string) : t = s

end

(* Pattern builders *)
module Pat = struct

  type t = PatRepr.t

  open PatRepr

  let any loc =
    mk loc Map.empty Ppat_any

  let var loc var =
    let var = Var.relocate var loc in
    let name = Var.name var in
    let map = Var.map var in
      mk loc map (Ppat_var name)

  let alias loc pat var =
    let var = Var.relocate var loc in
    let name = Var.name var in
    let map = Var.map var in
    let map, pat = merge loc map pat in
      mk loc map (Ppat_alias(pat, name))

  let constant loc const =
    mk loc Map.empty (Ppat_constant const)

  let interval loc const1 const2 =
    mk loc Map.empty (Ppat_interval(const1, const2))

  let tuple loc patl =
    let map, patl = accum_list merge loc Map.empty patl in
      mk loc map (Ppat_tuple patl)

  let construct loc lid pato =
    let map, pato = accum_option merge loc Map.empty pato in
      mk loc map (Ppat_construct(lid, pato))

  let variant loc label pato =
    let map, pato = accum_option merge loc Map.empty pato in
      mk loc map (Ppat_variant(label, pato))

  let record loc patl closed =
    let closed =
      if closed then CamlinternalAST.Closed else CamlinternalAST.Open
    in
    let map, patl = accum_alist merge loc Map.empty patl in
      mk loc map (Ppat_record(patl, closed))

  let array loc patl =
    let map, patl = accum_list merge loc Map.empty patl in
      mk loc map (Ppat_array patl)

  let or_ loc pat1 pat2 =
    let map, pat1, pat2 = join loc pat1 pat2 in
      mk loc map (Ppat_or(pat1, pat2))

  let type_ loc lid =
    mk loc Map.empty (Ppat_type lid)

  let lazy_ loc pat =
    let map, pat = merge loc Map.empty pat in
      mk loc map (Ppat_lazy pat)

  let exception_ loc pat =
    let map, pat = merge loc Map.empty pat in
      mk loc map (Ppat_exception pat)

end

module Case = struct

  type t = CaseRepr.t

  open CaseRepr

  let nonbinding loc lhs rhs =
    let lhs = PatRepr.nonbinding loc lhs in
    let heap, rhs = ExpRepr.merge loc Heap.empty rhs in
      mk heap lhs None rhs

  let simple loc name f =
    let heap, lhs, rhs = Binding.simple loc name f in
      mk heap lhs None rhs

  let pattern loc names f =
    let heap, lhs, rhs = Binding.pattern loc names f in
      mk heap lhs None rhs

  let guarded loc names f =
    let heap, lhs, guard, rhs = Binding.guarded loc names f in
      mk heap lhs (Some guard) rhs

end

module Exp = struct

  (* The representation of the possibly open code: AST plus the
     set of free identifiers, annotated with the related marks
  *)
  type t = ExpRepr.t

  module Closed = ExpRepr.Closed

  let to_closed = ExpRepr.to_closed

  let of_closed = ExpRepr.of_closed

  open ExpRepr

  let mk_vb loc p e =
    {pvb_pat = p;pvb_expr = e;pvb_loc = loc;pvb_attributes = [];}

  let var loc var =
    let heap = Var.heap var in
    let lid = Ident.of_var var in
      mk loc heap (Pexp_ident lid)

  let ident loc lid =
    mk loc Heap.empty (Pexp_ident lid)

  let constant loc const =
    mk loc Heap.empty (Pexp_constant const)

  let let_nonbinding loc pat def body =
    let pat = PatRepr.nonbinding loc pat in
    let heap, def = merge loc Heap.empty def in
    let heap, body = merge loc heap body in
    let vb = mk_vb loc pat def in
      mk loc heap (Pexp_let(Nonrecursive, [vb], body))

  let let_simple loc name def f =
    let heap, pat, body = Binding.simple loc name f in
    let heap, def = merge loc heap def in
    let vb = mk_vb loc pat def in
      mk loc heap (Pexp_let(Nonrecursive, [vb], body))

  let let_rec_simple loc names f =
    let heap, defs, body = Binding.recursive loc names f in
    let vbs = List.map (fun (pat, exp) -> mk_vb loc pat exp) defs in
      mk loc heap (Pexp_let(Recursive, vbs, body))

  let let_pattern loc names def f =
    let heap, pat, body = Binding.pattern loc names f in
    let heap, def = merge loc heap def in
    let vb = mk_vb loc pat def in
      mk loc heap (Pexp_let(Nonrecursive, [vb], body))

  let fun_nonbinding loc label pat exp =
    let pat = PatRepr.nonbinding loc pat in
    let heap, exp = merge loc Heap.empty exp in
      mk loc heap (Pexp_fun (label, None, pat, exp))

  let fun_simple loc name label default f =
    let heap, pat, exp = Binding.simple loc name f in
    let heap, default = accum_option merge loc heap default in
      mk loc heap (Pexp_fun (label, default, pat, exp))

  let fun_pattern loc names label default f =
    let heap, pat, exp = Binding.pattern loc names f in
    let heap, default = accum_option merge loc heap default in
      mk loc heap (Pexp_fun(label, default, pat, exp))

  let function_ loc cases =
    let heap, cases = accum_list CaseRepr.merge loc Heap.empty cases in
      mk loc heap (Pexp_function cases)

  let apply loc fn args =
    let heap, fn = merge loc Heap.empty fn in
    let heap, args = accum_alist merge loc heap args in
      mk loc heap (Pexp_apply (fn, args))

  let match_ loc exp cases =
    let heap, exp = merge loc Heap.empty exp in
    let heap, cases = accum_list CaseRepr.merge loc heap cases in
      mk loc heap (Pexp_match(exp, cases))

  let try_ loc exp cases =
    let heap, exp = merge loc Heap.empty exp in
    let heap, cases = accum_list CaseRepr.merge loc heap cases in
      mk loc heap (Pexp_try(exp, cases))

  let tuple loc exps =
    let heap, exps = accum_list merge loc Heap.empty exps in
      mk loc heap (Pexp_tuple exps)

  let construct loc lid argo =
    let heap, argo = accum_option merge loc Heap.empty argo in
      mk loc heap (Pexp_construct (lid, argo))

  let variant loc label argo =
    let heap, argo = accum_option merge loc Heap.empty argo in
      mk loc heap (Pexp_variant (label, argo))

  let record loc defs orig =
    let heap, defs = accum_alist merge loc Heap.empty defs in
    let heap, orig = accum_option merge loc heap orig in
      mk loc heap (Pexp_record (defs, orig))

  let field loc rcd lid =
    let heap, rcd = merge loc Heap.empty rcd in
      mk loc heap (Pexp_field (rcd, lid))

  let setfield loc rcd lid def =
    let heap, rcd = merge loc Heap.empty rcd in
    let heap, def = merge loc heap def in
      mk loc heap (Pexp_setfield (rcd, lid, def))

  let array loc args =
    let heap, args = accum_list merge loc Heap.empty args in
      mk loc heap (Pexp_array args)

  let ifthenelse loc cond tr fs =
    let heap, cond = merge loc Heap.empty cond in
    let heap, tr = merge loc heap tr in
    let heap, fs = accum_option merge loc heap fs in
      mk loc heap (Pexp_ifthenelse (cond, tr, fs))

  let sequence loc exp1 exp2 =
    let heap, exp1 = merge loc Heap.empty exp1 in
    let heap, exp2 = merge loc heap exp2 in
      mk loc heap (Pexp_sequence(exp1, exp2))

  let while_ loc cond body =
    let heap, cond = merge loc Heap.empty cond in
    let heap, body = merge loc heap body in
      mk loc heap (Pexp_while(cond, body))

  let for_ loc name low high dir f =
    let dir = if dir then CamlinternalAST.Upto else CamlinternalAST.Downto in
    let heap, pat, body = Binding.simple loc name f in
    let heap, low = merge loc heap low in
    let heap, high = merge loc heap high in
      mk loc heap (Pexp_for (pat, low, high, dir, body))

  let send loc obj meth =
    let heap, obj = merge loc Heap.empty obj in
      mk loc heap (Pexp_send(obj, meth))

  let assert_ loc exp =
    let heap, exp = merge loc Heap.empty exp in
      mk loc heap (Pexp_assert exp)

  let lazy_ loc exp =
    let heap, exp = merge loc Heap.empty exp in
      mk loc heap (Pexp_lazy exp)

  let open_ loc ovr lid exp =
    let ovr =
      if ovr then CamlinternalAST.Override else CamlinternalAST.Fresh
    in
    let heap, exp = merge loc Heap.empty exp in
      mk loc heap (Pexp_open(ovr, lid, exp))

  let quote loc exp =
    let heap, exp = merge loc Heap.empty exp in
      mk loc heap (Pexp_quote exp)

  let escape loc exp =
    let heap, exp = merge loc Heap.empty exp in
      mk loc heap (Pexp_escape exp)

end
