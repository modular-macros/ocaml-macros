(* TEST
 readonly_files = "splice_object_flags_lib.ml";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 flags = "-linkall";
 all_modules = "\
   splice_object_flags_lib.ml \
   splice_object_flags.ml";
 ocamlc.byte;
 program = "splice_object_flags.cmo";
 ocamlobjinfo;
 check-program-output;
*)

(* A unit with a top-level splice has its object written by its static
   program, a separate process whose Clflags hold their DEFAULTS, so every
   flag the emitter consults was dropped for exactly those units: -g
   produced an object byte-identical to one without it, -linkall a unit the
   linker was free to drop, -for-pack an object the later -pack step then
   refused.  The primitive table went empty for a neighbouring reason --
   the external declarations accumulate during translation, in the parent.

   Below, "Force link: YES" comes from -linkall and the primitive from the
   external; both are written by the child.  The flags travel as arguments
   of the emit call (Clflags.emitter_flags), which is the only channel a
   module called by field offset has. *)

external ident : int -> int = "%identity"

let x = $(Splice_object_flags_lib.zero ())
let () = ignore (ident x)
