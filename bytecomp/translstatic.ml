(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Olivier Nicole, Paris-Saclay University                  *)
(*                                                                        *)
(*                      Copyright 2016 OCaml Labs                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Lambda

let transl_implementation module_name str (* module coercion unhandled *) =
  transl_structure str.str_items

let rec transl_structure = function
  | [] -> lambda_unit
  | item :: rem ->
    begin
      match item.str_desc with
      | Tstr_value(Static, rec_flag, pat_expr_list) ->
          let body = transl_structure rem in
          transl_let rec_flag pat_expr_list body
      | _ -> (* all non-static bindings are ignored *)
          transl_structure rem
    end

