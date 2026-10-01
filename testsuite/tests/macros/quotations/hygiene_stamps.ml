(* TEST
 flags = "-w -unused-var";
 bytecode;
*)

(* A binder that is renamed when its quotation is evaluated never
   takes the stamp of an identifier of the compiler that a code value
   mentions: renamed binders have negative stamps.  The splice evaluates
   quotations until a renamed binder would reach the stamp of [x], were
   the stamps to count upwards, and the variable would then be
   captured.  The test reads the stamp of [x] from the code that the
   bytecode compiler builds for it. *)

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
