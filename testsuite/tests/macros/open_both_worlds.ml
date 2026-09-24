(* TEST
 readonly_files = "open_both_worlds_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "open_both_worlds_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "open_both_worlds.ml";
   flags = "-I . -I-static . -static-use open_both_worlds_lib.cmo";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "open_both_worlds_lib.cmo open_both_worlds.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* An open serves what its head serves.  A name reached through
   [open L] resolves exactly as [L.name] would: the same per-member
   world selection a qualified path gets ([member_stage_shift]) is
   applied to the open layer's candidate, instead of the layer's own
   shift -- fixed when the open was typed -- being the only one tried.

   The library is registered for BOTH worlds (-I and -I-static), so
   through one structure-level [open] its run-time values serve level
   0 in run-time code and level -1 in a generator body (previously:
   "run is bound at run time, but this use is in compile-time code",
   while [Lib.run] compiled a line later); and a name QUOTED through
   a level -1 open of the unit resolves in the run world, as
   [<< Lib.seven >>] does (previously: "bound at run time, but this
   use is at run time", the layer's -1 shift making the level-0 use
   miss). *)

open Open_both_worlds_lib

module M = struct $let v = 41 end

(* The report: a run-time value, through a level-0 open, at -1. *)
$gen a () : int expr = Expr.int (run (module M : S))

(* The older report: a name quoted through a -1 open. *)
$gen b () : int expr = let open Open_both_worlds_lib in << id seven >>

(* The same open, in run-time code. *)
module R = struct let v = 1 end
let c = run (module R : S)

let () = Printf.printf "%d %d %d\n" $(a ()) $(b ()) c
