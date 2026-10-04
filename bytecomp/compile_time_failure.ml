(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let message what = function
  | Effect.Unhandled (CamlinternalQuote.Identifier.FreeVar fvs) ->
      (* A variable that holds evidence escapes with code that uses its
         equation. *)
      let evidence, names =
        List.partition (String.equal Env.evidence_name)
          (CamlinternalQuote.Identifier.names fvs)
      in
      let variables =
        match names with
        | [] -> []
        | _ ->
            [Printf.sprintf "has %s escaping a quotation's scope: %s"
               (match names with [_] -> "a variable" | _ -> "variables")
               (String.concat ", " names)]
      in
      let equations =
        match evidence with
        | [] -> []
        | _ -> ["uses a type equation outside the match case where it holds"]
      in
      Printf.sprintf "The code built by %s %s." what
        (String.concat ", and " (variables @ equations))
  | exn ->
      Printf.sprintf "Compile-time evaluation of %s raised %s."
        what (Printexc.to_string exn)
