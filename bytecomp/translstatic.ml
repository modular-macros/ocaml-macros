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

let rec transl_implementation str (* module coercion unhandled *) =
  transl_structure str.str_items

and transl_structure = function
  | [] -> lambda_unit
  | item :: rem ->
    begin
      match item.str_desc with
      | Tstr_value(Asttypes.Static, rec_flag, pat_expr_list) ->
          let body = transl_structure rem in
          Translcore.transl_let rec_flag pat_expr_list body
      | _ -> (* everything but static bindings is ignored *)
          transl_structure rem
    end

