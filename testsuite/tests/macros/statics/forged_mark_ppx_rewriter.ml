(* A rewriter that marks every variable of a pattern as a static
   binding *)

open Parsetree

let mapper =
  { Ast_mapper.default_mapper with
    pat = (fun mapper p ->
      let p = Ast_mapper.default_mapper.pat mapper p in
      match p.ppat_desc with
      | Ppat_var _ ->
          let mark =
            Ast_helper.Attr.mk (Location.mknoloc "ocaml.static")
              (PStr [])
          in
          { p with ppat_attributes = mark :: p.ppat_attributes }
      | _ -> p) }

let () = Ast_mapper.run_main (fun _ -> mapper)
