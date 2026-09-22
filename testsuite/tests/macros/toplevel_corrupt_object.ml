(* TEST
 readonly_files = "corrupt_macro_object.truncated";
 setup-ocamlc.byte-build-env;
 (* The backslash protects the $ from ocamltest's variable expansion. *)
 script = "cp corrupt_macro_object.truncated \
           toplevel_corrupt_object_trunc.cmo";
 script;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* A truncated object read by the toplevel.  The magic read sat outside
   every handler, so [End_of_file] reached the top loop, which cannot tell
   it from the user's end of input: the session ended with status 0 and no
   message at all.  Worse, the file need never be named -- #load probes for
   a unit's $macros twin by itself, so a truncated twin ended the session
   on a #load of the unit beside it.

   Both readers now report and carry on, which is what every other
   malformed input in the toplevel does.  The last phrase is the point of
   the test: it has to be reached. *)

#load "toplevel_corrupt_object_trunc.cmo";;

#static_use "toplevel_corrupt_object_trunc.cmo";;

let survived = 1 + 1;;
