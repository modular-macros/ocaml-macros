(* A rewriter that gives every template application and every include
   the location of the first that it meets *)

open Parsetree

let first = ref None

let fix loc =
  match !first with
  | None -> first := Some loc; loc
  | Some loc0 -> loc0

let mapper =
  { Ast_mapper.default_mapper with
    module_expr = (fun m me ->
      let me = Ast_mapper.default_mapper.module_expr m me in
      match me.pmod_desc with
      | Pmod_apply (Asttypes.Template, _, _) ->
          { me with pmod_loc = fix me.pmod_loc }
      | _ -> me);
    structure_item = (fun m si ->
      let si = Ast_mapper.default_mapper.structure_item m si in
      match si.pstr_desc with
      | Pstr_include i ->
          { pstr_desc = Pstr_include { i with pincl_loc = fix i.pincl_loc };
            pstr_loc = fix si.pstr_loc }
      | _ -> si) }

let () = Ast_mapper.run_main (fun _ -> mapper)
