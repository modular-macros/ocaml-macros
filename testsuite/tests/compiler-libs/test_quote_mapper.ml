(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

(* A mapper and an iterator reach the expressions under a quotation and
   under a splice. *)

let parse () =
  Parse.implementation
    (Lexing.from_string "let a = << f $x $M.y $(g << z >>) >>")

let renamed =
  let open Ast_mapper in
  let expr mapper (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident ({ txt = Longident.Lident s; _ } as lid) ->
        { e with pexp_desc =
                   Pexp_ident { lid with txt = Longident.Lident (s ^ "'") } }
    | _ -> default_mapper.expr mapper e
  in
  let mapper = { default_mapper with expr } in
  Format.asprintf "%a" Pprintast.structure (mapper.structure mapper (parse ()))
;;
[%%expect{|
val parse : unit -> Parsetree.structure = <fun>
val renamed : string = "let a = << f' $(x') $(M.y) $(g' << z' >>) >>"
|}]

let identifiers =
  let open Ast_iterator in
  let found = ref [] in
  let expr iterator (e : Parsetree.expression) =
    (match e.pexp_desc with
     | Pexp_ident { txt; _ } ->
         found := String.concat "." (Longident.flatten txt) :: !found
     | _ -> ());
    default_iterator.expr iterator e
  in
  let iterator = { default_iterator with expr } in
  iterator.structure iterator (parse ());
  List.rev !found
;;
[%%expect{|
val identifiers : string list = ["f"; "x"; "M.y"; "g"; "z"]
|}]
