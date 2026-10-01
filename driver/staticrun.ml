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

(* The program that runs the static program of a unit.  It is linked
   once, with the compiler, and contains the standard library, the
   libraries of the compiler and Dynlink.  Called as

     staticrun <base> <objects-file>

   it loads the objects that the lines of <objects-file> name, in order.
   The last is the static program, whose initialisation is its
   evaluation.  A failure to load is written to <base>.linkerr. *)

let unit_of_object file =
  String.capitalize_ascii Filename.(remove_extension (basename file))

let report base msg =
  let oc = open_out_bin (base ^ ".linkerr") in
  output_string oc msg;
  close_out oc;
  exit 3

let read_objects file =
  let ic = open_in file in
  let rec go acc =
    match input_line ic with
    | "" -> go acc
    | line -> go (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  go []

let () =
  match Sys.argv with
  | [| _; base; objs_file |] ->
      let objects =
        try read_objects objs_file
        with Sys_error msg -> report base msg
      in
      Dynlink.allow_unsafe_modules true;
      List.iter
        (fun f ->
           try Dynlink.loadfile f with
           | Dynlink.Error (Dynlink.Unavailable_unit g)
           | Dynlink.Error
               (Dynlink.Linking_error (_, Dynlink.Undefined_global g)) ->
               report base
                 (Printf.sprintf "%s referenced from %s" g
                    (unit_of_object f))
           | Dynlink.Error e -> report base (Dynlink.error_message e)
           | Sys_error msg -> report base msg)
        objects
  | _ ->
      prerr_endline "usage: staticrun <base> <objects-file>";
      exit 4
