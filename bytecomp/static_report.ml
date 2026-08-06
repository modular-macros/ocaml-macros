(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let message what = function
  | Effect.Unhandled (CamlinternalQuote.Identifier.FreeVar fvs) ->
      let names = CamlinternalQuote.Identifier.names fvs in
      Printf.sprintf
        "The code built by %s has %s escaping a quotation's scope: %s."
        what
        (match names with [_] -> "a variable" | _ -> "variables")
        (String.concat ", " names)
  | exn ->
      Printf.sprintf "Compile-time evaluation of %s raised %s."
        what (Printexc.to_string exn)

let run err_file loc what f =
  try f () with
  | exn ->
      let msg = message what exn in
      let oc = open_out_bin err_file in
      Fun.protect ~finally:(fun () -> close_out oc)
        (fun () -> Marshal.to_channel oc ((loc, msg) : Location.t * string) []);
      exit 2
