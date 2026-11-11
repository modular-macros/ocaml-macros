open Typedtree
type site = { modname : string; name : string; kind : Asttypes.functor_kind; file : string;
              line : int }

type t = { path : string; modname : string; definitions : site list; applications : site list }

let name_of_path modname path =
  match String.split_on_char '.' (Path.name path) with
  | lib :: m :: rest when String.ends_with ~suffix:"__" lib -> String.concat "." ((lib ^ m) :: rest)
  | parts when Ident.global (Path.head path) -> String.concat "." parts
  | parts -> String.concat "." (modname :: parts)

let rec path_of me =
  match me.mod_desc with
  | Tmod_ident (path, _) -> Some path
  | Tmod_constraint (me, _, _, _) -> path_of me
  | _ -> None

let rec functor_kind me =
  match me.mod_desc with
  | Tmod_functor (kind, _, _) -> Some kind
  | Tmod_constraint (me, _, _, _) -> functor_kind me
  | _ -> None

let application_kind = function
  | Plain_application -> Asttypes.Plain
  | Template_application _ -> Asttypes.Template

let scan modname file (structure : structure) =
  let definitions = ref [] and applications = ref [] and prefix = ref [ modname ] in
  let mk_site name kind (loc : Location.t) = { modname; name; kind; file; line = loc.loc_start.pos_lnum } in
  let module_binding it mb =
    match mb.mb_id with
    | None -> Tast_iterator.default_iterator.module_binding it mb
    | Some id ->
        let name = Ident.name id in
        Option.iter (fun kind ->
            let qualified = String.concat "." (List.rev (name :: !prefix)) in
            definitions := mk_site qualified kind mb.mb_loc :: !definitions)
          (functor_kind mb.mb_expr);
        prefix := name :: !prefix;
        Tast_iterator.default_iterator.module_binding it mb;
        prefix := List.tl !prefix
  in
  let module_expr it me =
    (match me.mod_desc with
     | Tmod_apply (app, funct, _, _) | Tmod_apply_unit (app, funct) ->
         Option.iter (fun path ->
             let kind = application_kind app in
             applications := mk_site (name_of_path modname path) kind me.mod_loc :: !applications)
           (path_of funct)
     | _ -> ());
    Tast_iterator.default_iterator.module_expr it me
  in
  let it = { Tast_iterator.default_iterator with module_binding; module_expr } in
  it.structure it structure;
  (List.rev !definitions, List.rev !applications)

let read path =
  let cmt = Cmt_format.read_cmt path in
  let modname = cmt.cmt_modname in
  let definitions, applications =
    match cmt.cmt_annots with
    | Implementation s -> scan modname (Option.value cmt.cmt_sourcefile ~default:path) s
    | _ -> ([], [])
  in
  { path; modname; definitions; applications }

let site_header = [ "role"; "module"; "name"; "kind"; "file"; "line" ]

let site_row role { modname; name; kind; file; line } =
  let kind = match kind with Asttypes.Plain -> "functor" | Asttypes.Template -> "template" in
  [ role; modname; name; kind; file; string_of_int line ]

(* Appends to PREFIX-units.csv and PREFIX-sites.csv. *)
let record ~prefix context { path; modname; definitions; applications } =
  let units = Table.configure ~out:(prefix ^ "-units.csv") context
  and sites = Table.configure ~out:(prefix ^ "-sites.csv") context in
  Table.append units ~header:[ "path"; "module" ] [ path; modname ];
  let append role site = Table.append sites ~header:site_header (site_row role site) in
  List.iter (append "definition") definitions;
  List.iter (append "application") applications

let () =
  match Array.to_list Sys.argv with
  | _ :: prefix :: args ->
      let context, files = Table.of_args args in
      List.iter (fun path -> record ~prefix context (read path)) files
  | _ -> prerr_endline "usage: cmt PREFIX COLUMN=VALUE ... -- FILE.cmt ..."; exit 2
