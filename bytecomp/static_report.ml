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

type failure = Location.t * string

let run err_file loc what f =
  try f () with
  | exn ->
      let failure = loc, Compile_time_failure.message what exn in
      let oc = open_out_bin err_file in
      Fun.protect ~finally:(fun () -> close_out oc)
        (fun () -> Marshal.to_channel oc (failure : failure) []);
      exit 2
