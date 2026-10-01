(* TEST
 include ocamlcommon;
 arguments = "${ocamlsrcdir}/bytecomp/static_holes.cmi \
              ${ocamlsrcdir}/bytecomp/static_report.cmi";
 bytecode;
*)

(* The static program of a unit calls a function of each of these units
   by its position in the unit's block, 0: the block of each has no
   other component. *)

let components file =
  List.filter_map
    (function
      | Types.Sig_value (_, { val_kind = Val_prim _; _ }, _) -> None
      | Types.Sig_value (id, _, _) | Types.Sig_typext (id, _, _, _)
      | Types.Sig_module (id, Mp_present, _, _, _)
      | Types.Sig_class (id, _, _, _) ->
          Some (Ident.name id)
      | _ -> None)
    (Cmi_format.read_cmi file).cmi_sign

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    Printf.printf "%s: %s\n"
      (Filename.basename file) (String.concat ", " (components file))
  done
