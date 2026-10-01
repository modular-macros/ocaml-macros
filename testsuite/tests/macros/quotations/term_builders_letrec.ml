(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The builders var and letrec of CamlinternalLam make a recursive
   binding that no quotation of fixed shape expresses.  The identifier
   of the binding is made by hand. *)

let fact : int -> int =
  $(let pack (l : CamlinternalLam.lam) : 'a expr =
      Obj.magic (l, CamlinternalQuote.Identifier.empty)
    and unpack (e : 'a expr) : CamlinternalLam.lam =
      fst (Obj.magic e : CamlinternalLam.lam * Obj.t)
    in
    let f =
      CamlinternalLam.Ident.Local { name = "fact"; stamp = 1_000_000_007 }
    in
    let call : (int -> int) expr = pack (CamlinternalLam.var f) in
    let def =
      << fun n -> if n = 0 then 1 else n * $call (n - 1) >> in
    pack (CamlinternalLam.letrec [ (f, unpack def) ] (unpack call)))

let () = print_int (fact 5); print_newline ()
