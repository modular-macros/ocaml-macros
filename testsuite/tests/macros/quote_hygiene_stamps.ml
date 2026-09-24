(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* The quote runtime's rename counter must never mint a stamp that
   collides with a compiler ident baked into a code value: a renamed
   binder equal to a baked free identifier would make the scope
   machinery treat the free variable as bound -- the extrusion check
   silently passes and the built term CAPTURES.  Runtime stamps count
   downward (negative), compiler stamps are strictly positive, so the
   chase below can never converge; before that fix this test printed
   1000000.  The probe steers with Obj only to AIM: the natural
   exposure was any compilation whose macros mint a few hundred
   binders. *)

let x = 7

let captured = $(
  let c = << x >> in
  let local (i : CamlinternalLam.Ident.t) =
    match i with
    | CamlinternalLam.Ident.Local { name = _; stamp } -> stamp
    | _ -> failwith "not Local" in
  let code = Obj.field (Obj.repr c) 0 in         (* the Lvar block *)
  let xid : CamlinternalLam.Ident.t = Obj.obj (Obj.field code 0) in
  let m = local xid in
  (* One rename per build; returns the stamp it consumed. *)
  let probe () =
    let q = << let v = 0 in 1 >> in
    let qcode = Obj.field (Obj.repr q) 0 in      (* the Llet block *)
    local (Obj.obj (Obj.field qcode 2)) in
  let cur = ref (probe ()) in
  let attempts = ref 0 in
  while !cur < m - 1 && !attempts < 5000 do
    cur := probe (); incr attempts
  done;
  << let x = 1000000 in $c + 0 >> )

let () = print_int captured; print_newline ()
