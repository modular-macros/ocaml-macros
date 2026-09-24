(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/lambda";
 include ocamlcommon;
 expect;
*)

(* The quote machinery builds and consumes [Lambda.lambda] values at the
   Obj level, and stdlib/camlinternalLam.ml mirrors [Ident.t] and
   (privately) [structured_constant]; see the layout notes at
   [type lambda] and [type structured_constant] in lambda.mli, and
   O3.md.  This test pins the correspondence:
   - the exhaustive matches below stop compiling when a constructor is
     added to [lambda] or to the mirror, forcing a visit here;
   - the tag/size table catches reordered or re-fielded constructors;
   - the marker probes catch swapped fields of the records;
   - the Ident roster checks the mirror agrees with the compiler, shape
     and ordering both, for every constructor.
   If this test fails, update Translquote and camlinternalLam before
   updating the expectations, and bump the CMO magic number. *)

open Lambda

(* Tripwire: a new [lambda] constructor must be added here, to the table
   below, and to Translquote's quoters. *)
let constructor_index : lambda -> int = function
  | Lvar _ -> 0 | Lmutvar _ -> 1 | Lconst _ -> 2 | Lapply _ -> 3
  | Lfunction _ -> 4 | Llet _ -> 5 | Lmutlet _ -> 6 | Lletrec _ -> 7
  | Lprim _ -> 8 | Lswitch _ -> 9 | Lstringswitch _ -> 10
  | Lstaticraise _ -> 11 | Lstaticcatch _ -> 12 | Ltrywith _ -> 13
  | Lifthenelse _ -> 14 | Lsequence _ -> 15 | Lwhile _ -> 16
  | Lfor _ -> 17 | Lassign _ -> 18 | Lsend _ -> 19 | Levent _ -> 20
  | Lifused _ -> 21 | Lsplice _ -> 22
;;
[%%expect{|
val constructor_index : Lambda.lambda -> int = <fun>
|}]

let id = Ident.create_local "marker_id"
let u = lambda_unit
let ev = { lev_loc = Debuginfo.Scoped_location.Loc_unknown;
           lev_kind = Lev_before; lev_repr = None; lev_env = Env.empty }
let sw = { sw_numconsts = 7001; sw_consts = []; sw_numblocks = 7002;
           sw_blocks = []; sw_failaction = None }
let app = { ap_func = u; ap_args = []; ap_loc = Loc_unknown;
            ap_tailcall = Default_tailcall; ap_inlined = Default_inline;
            ap_specialised = Default_specialise }
let fn = lfunction ~kind:Curried ~params:[ (id, Pgenval) ] ~return:Pgenval
           ~body:u ~attr:default_function_attribute ~loc:Loc_unknown

let specimens = [
  Lvar id; Lmutvar id; Lconst const_unit; Lapply app; fn;
  Llet (Strict, Pgenval, id, u, u); Lmutlet (Pgenval, id, u, u);
  Lletrec ([], u); Lprim (Pignore, [ u ], Loc_unknown);
  Lswitch (u, sw, Loc_unknown); Lstringswitch (u, [], None, Loc_unknown);
  Lstaticraise (7003, []); Lstaticcatch (u, (7004, []), u);
  Ltrywith (u, id, u); Lifthenelse (u, u, u); Lsequence (u, u);
  Lwhile (u, u); Lfor (id, u, u, Upto, u); Lassign (id, u);
  Lsend (Public, u, u, [], Loc_unknown); Levent (u, ev); Lifused (id, u);
  Lsplice u ]

(* Expected Obj size of each constructor's block, in declaration order. *)
let sizes = [ 1; 1; 1; 1; 1; 5; 4; 2; 3; 3; 4; 2; 3; 3; 3; 2; 2; 5; 2;
              5; 2; 2; 1 ]

(* The recorded value below must stay []: the runner does not capture
   stdout into the expectation, so drift is reported as data. *)
let lambda_drift =
  List.concat
    (List.mapi
       (fun i (lam, size) ->
          let o = Obj.repr lam in
          assert (constructor_index lam = i);
          (if Obj.tag o <> i then
             [ Printf.sprintf "constructor %d: tag %d" i (Obj.tag o) ]
           else [])
          @ (if Obj.size o <> size then
               [ Printf.sprintf "constructor %d: size %d, expected %d"
                   i (Obj.size o) size ]
             else []))
       (List.combine specimens sizes))
;;
[%%expect{|
val id : Ident.t = <abstr>
val u : Lambda.lambda = Lconst (Const_int 0)
val ev : Lambda.lambda_event =
  {lev_loc = Debuginfo.Scoped_location.Loc_unknown; lev_kind = Lev_before;
   lev_repr = None; lev_env = <abstr>}
val sw : Lambda.lambda_switch =
  {sw_numconsts = 7001; sw_consts = []; sw_numblocks = 7002; sw_blocks = [];
   sw_failaction = None}
val app : Lambda.lambda_apply =
  {ap_func = Lconst (Const_int 0); ap_args = [];
   ap_loc = Debuginfo.Scoped_location.Loc_unknown;
   ap_tailcall = Default_tailcall; ap_inlined = Default_inline;
   ap_specialised = Default_specialise}
val fn : Lambda.lambda =
  Lfunction
   {kind = Curried; params = [(<abstr>, Pgenval)]; return = Pgenval;
    body = Lconst (Const_int 0);
    attr =
     {inline = Default_inline; specialise = Default_specialise;
      local = Default_local; poll = Default_poll; is_a_functor = false;
      stub = false; tmc_candidate = false; may_fuse_arity = true};
    loc = Debuginfo.Scoped_location.Loc_unknown}
val specimens : Lambda.lambda list =
  [Lvar <abstr>; Lmutvar <abstr>; Lconst (Const_int 0);
   Lapply
    {ap_func = Lconst (Const_int 0); ap_args = [];
     ap_loc = Debuginfo.Scoped_location.Loc_unknown;
     ap_tailcall = Default_tailcall; ap_inlined = Default_inline;
     ap_specialised = Default_specialise};
   Lfunction
    {kind = Curried; params = [(<abstr>, Pgenval)]; return = Pgenval;
     body = Lconst (Const_int 0);
     attr =
      {inline = Default_inline; specialise = Default_specialise;
       local = Default_local; poll = Default_poll; is_a_functor = false;
       stub = false; tmc_candidate = false; may_fuse_arity = true};
     loc = Debuginfo.Scoped_location.Loc_unknown};
   Llet (Strict, Pgenval, <abstr>, Lconst (Const_int 0),
    Lconst (Const_int 0));
   Lmutlet (Pgenval, <abstr>, Lconst (Const_int 0), Lconst (Const_int 0));
   Lletrec ([], Lconst (Const_int 0));
   Lprim (Pignore, [Lconst (Const_int 0)],
    Debuginfo.Scoped_location.Loc_unknown);
   Lswitch (Lconst (Const_int 0),
    {sw_numconsts = 7001; sw_consts = []; sw_numblocks = 7002;
     sw_blocks = []; sw_failaction = None},
    Debuginfo.Scoped_location.Loc_unknown);
   Lstringswitch (Lconst (Const_int 0), [], None,
    Debuginfo.Scoped_location.Loc_unknown);
   Lstaticraise (7003, []);
   Lstaticcatch (Lconst (Const_int 0), (7004, []), Lconst (Const_int 0));
   Ltrywith (Lconst (Const_int 0), <abstr>, Lconst (Const_int 0));
   Lifthenelse (Lconst (Const_int 0), Lconst (Const_int 0),
    Lconst (Const_int 0));
   Lsequence (Lconst (Const_int 0), Lconst (Const_int 0));
   Lwhile (Lconst (Const_int 0), Lconst (Const_int 0));
   Lfor (<abstr>, Lconst (Const_int 0), Lconst (Const_int 0), Asttypes.Upto,
    Lconst (Const_int 0));
   Lassign (<abstr>, Lconst (Const_int 0));
   Lsend (Public, Lconst (Const_int 0), Lconst (Const_int 0), [],
    Debuginfo.Scoped_location.Loc_unknown);
   Levent (Lconst (Const_int 0),
    {lev_loc = Debuginfo.Scoped_location.Loc_unknown; lev_kind = Lev_before;
     lev_repr = None; lev_env = <abstr>});
   Lifused (<abstr>, Lconst (Const_int 0)); Lsplice (Lconst (Const_int 0))]
val sizes : int list =
  [1; 1; 1; 1; 1; 5; 4; 2; 3; 3; 4; 2; 3; 3; 3; 2; 2; 5; 2; 5; 2; 2; 1]
val lambda_drift : string list = []
|}]

(* Field-position probes: markers must sit where the quoters put them. *)
let field n v = Obj.field (Obj.repr v) n
let () =
  (* Llet: let_kind, value_kind, id, e1, e2 *)
  assert (field 2 (Llet (Strict, Pgenval, id, u, u)) == Obj.repr id);
  (* Lfor: id, lo, hi, dir, body *)
  assert (field 0 (Lfor (id, u, u, Upto, u)) == Obj.repr id);
  (* lambda_apply: ap_func first *)
  assert (field 0 (Obj.magic (Lapply app) : Obj.t) == Obj.repr app);
  assert (field 0 app == Obj.repr u);
  (* lambda_switch: sw_numconsts, sw_consts, sw_numblocks, ... *)
  assert ((Obj.obj (field 0 sw) : int) = 7001);
  assert ((Obj.obj (field 2 sw) : int) = 7002);
  assert (Obj.size (Obj.repr sw) = 5);
  assert (Obj.size (Obj.repr app) = 6);
  (* lfunction: kind, params, return, body, attr, loc *)
  (match fn with
   | Lfunction f ->
       assert (Obj.size (Obj.repr f) = 6);
       (match Obj.obj (field 1 f) with
        | [ (i, _) ] -> assert (i == id)
        | _ -> assert false)
   | _ -> assert false);
  (* rec_binding: id, def *)
  (match fn with
   | Lfunction f ->
       let rb = { id; def = f } in
       assert (Obj.size (Obj.repr rb) = 2);
       assert (field 0 rb == Obj.repr id)
   | _ -> assert false);
  (* lambda_event: lev_loc, lev_kind, lev_repr, lev_env *)
  assert (Obj.size (Obj.repr ev) = 4)
;;
[%%expect{|
val field : int -> 'a -> Obj.t = <fun>
|}]

(* The Ident mirror: every compiler constructor, shape and ordering. *)
let mirror (i : Ident.t) : CamlinternalLam.Ident.t = Obj.magic i

(* Tripwire: a new mirror constructor must be added here and to the
   roster below; a new compiler [Ident.t] constructor must be added to
   the mirror (this cannot be observed here -- [Ident.t] is abstract --
   which is what the warning at its definition in typing/ident.ml is
   for). *)
let mirror_kind : CamlinternalLam.Ident.t -> string =
  let open CamlinternalLam.Ident in
  function
  | Local _ -> "Local" | Scoped _ -> "Scoped" | Global _ -> "Global"
  | Predef _ -> "Predef" | Unscoped _ -> "Unscoped"

let roster =
  [ Ident.create_local "loc_a";
    Ident.create_local "loc_b";
    Ident.create_scoped ~scope:3 "sco";
    Ident.create_persistent "Per";
    Ident.create_predef "pre";
    Ident.of_unscoped (Ident.Unscoped.create "uns") ]

let () =
  List.iter
    (fun i ->
       let m = mirror i in
       (match m with
        | Local { name; stamp = _ } | Scoped { name; stamp = _; scope = _ }
        | Global name | Predef { name; stamp = _ } ->
            assert (name = Ident.name i)
        | Unscoped us ->
            assert (CamlinternalLam.Ident.Unscoped.
                      (get_desc us).name = Ident.name i));
       ignore (mirror_kind m))
    roster;
  (* Ordering: the mirror's compare must agree with the compiler's on
     every pair. *)
  let sign c = compare c 0 in
  List.iter
    (fun a ->
       List.iter
         (fun b ->
            assert (sign (CamlinternalLam.Ident.compare (mirror a) (mirror b))
                    = sign (Ident.compare a b)))
         roster)
    roster
;;
[%%expect{|
val mirror : Ident.t -> CamlinternalLam.Ident.t = <fun>
val mirror_kind : CamlinternalLam.Ident.t -> string = <fun>
val roster : Ident.t list =
  [<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>]
|}]

(* The [structured_constant] mirror, private to camlinternalLam.ml, is
   pinned through [CamlinternalLam.const]: its runtime-built [Lconst]
   node must be structurally identical to this compiler's own -- deep
   polymorphic comparison covers the constructor tags and field shapes
   of every constant the classification produces.  (A char classifies
   as its immediate, so [Const_char] is never produced, but it must
   stay declared in the mirror for the tags after it; the tripwire
   below forces a visit when the compiler type changes.) *)
let sc_index : structured_constant -> int = function
  | Const_int _ -> 0 | Const_char _ -> 1 | Const_float _ -> 2
  | Const_int32 _ -> 3 | Const_int64 _ -> 4 | Const_nativeint _ -> 5
  | Const_block _ -> 6 | Const_float_array _ -> 7 | Const_immstring _ -> 8

let lifted (v : 'a) : lambda = Obj.magic (CamlinternalLam.const v)

let const_drift =
  let check (got, want) =
    if compare got (Lconst want) = 0 then []
    else [ Printf.sprintf "constant %d drifted" (sc_index want) ]
  in
  List.concat_map check
    [ lifted 42, Const_int 42;
      lifted 'q', Const_int (Char.code 'q');
      lifted 1.5, Const_float "0x1.8p+0";
      lifted (-3l), Const_int32 (-3l);
      lifted 9_000_000_000L, Const_int64 9_000_000_000L;
      lifted 5n, Const_nativeint 5n;
      lifted (Some [ 1; 2 ]),
        Const_block (0, [ Const_block (0, [ Const_int 1;
          Const_block (0, [ Const_int 2; Const_int 0 ]) ]) ]);
      lifted "mirror", Const_immstring "mirror" ]
;;
[%%expect{|
val sc_index : Lambda.structured_constant -> int = <fun>
val lifted : 'a -> Lambda.lambda = <fun>
val const_drift : string list = []
|}]

(* Flat float blocks -- a float array, a [floatarray], an all-float
   record -- all classify as [Const_float_array]; an empty float
   array is a zero-size block with tag 0, i.e. an empty
   [Const_block]. *)
type fr = { fx : float; fy : float }

let float_block_drift =
  let check (got, want) =
    if compare got (Lconst want) = 0 then []
    else [ Printf.sprintf "constant %d drifted" (sc_index want) ]
  in
  List.concat_map check
    [ lifted [| 1.5; 2.5 |], Const_float_array [ "0x1.8p+0"; "0x1.4p+1" ];
      lifted (Float.Array.of_list [ 0.5 ]), Const_float_array [ "0x1p-1" ];
      lifted { fx = 1.5; fy = 0.5 },
        Const_float_array [ "0x1.8p+0"; "0x1p-1" ];
      lifted ([||] : float array), Const_block (0, []) ]
;;
[%%expect{|
type fr = { fx : float; fy : float; }
val float_block_drift : string list = []
|}]

(* The term builders [CamlinternalLam.var] and [CamlinternalLam.letrec]
   (serving the letrec library) construct [Lvar] and [Lletrec] nodes as
   raw blocks, [letrec] unwrapping each [Lfunction] right-hand side
   into a [rec_binding]; they must coincide with the compiler's own
   construction, and [letrec] must reject a right-hand side that is not
   a syntactic function. *)
let builder_drift =
  let lam (l : lambda) : CamlinternalLam.lam = Obj.magic l in
  let mid : CamlinternalLam.Ident.t = Obj.magic id in
  (match (Obj.magic (CamlinternalLam.var mid) : lambda) with
   | Lvar i when i == id -> []
   | _ -> [ "var built a different term" ])
  @ (match (Obj.magic (CamlinternalLam.letrec [ (mid, lam fn) ] (lam u))
            : lambda) with
     | Lletrec ([ { id = i; def } ], b)
       when i == id && b == u
            && (match fn with
                | Lfunction f -> Obj.repr def == Obj.repr f
                | _ -> false) -> []
     | _ -> [ "letrec built a different term" ])
  @ (match CamlinternalLam.letrec [ (mid, lam u) ] (lam u) with
     | exception Invalid_argument _ -> []
     | _ -> [ "letrec accepted a non-function right-hand side" ])
;;
[%%expect{|
val builder_drift : string list = []
|}]

(* [CamlinternalLam.make_iarray] (serving [Expr.iarray]) constructs
   [Lprim (Pmakearray (Pgenarray, Immutable), els, Loc_unknown)] as a
   raw block -- [Lprim]'s tag among [lambda]'s constructors,
   [Pmakearray]'s among [primitive]'s argument-carrying constructors,
   and the three immediate-[0] payload constructors are all pinned
   here; the empty case must be the constant empty array. *)
let make_iarray_drift =
  let lam (l : lambda) : CamlinternalLam.lam = Obj.magic l in
  let e = Lvar id in
  (match (Obj.magic (CamlinternalLam.make_iarray [ lam u; lam e ])
          : lambda) with
   | Lprim (Pmakearray (Pgenarray, Immutable), [ a; b ], Loc_unknown)
     when a == u && b == e -> []
   | _ -> [ "make_iarray built a different term" ])
  @ (match (Obj.magic (CamlinternalLam.make_iarray []) : lambda) with
     | Lconst (Const_block (0, [])) -> []
     | _ -> [ "make_iarray [] is not the empty constant array" ])
;;
[%%expect{|
val make_iarray_drift : string list = []
|}]
