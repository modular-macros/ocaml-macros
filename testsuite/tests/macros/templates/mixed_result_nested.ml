(* TEST
 readonly_files = "mixed_result_nested_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "mixed_result_nested_lib.ml \
     mixed_result_nested.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "mixed_result_nested_lib.ml \
     mixed_result_nested.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Mixed results of in-body applications, nested mode: projecting and
   splicing the nested instantiation's macros through the functor's
   applications, across two instantiations of the template
   -- per-instantiation records, per-application run-time blocks. *)

open Mixed_result_nested_lib

module MI = struct $let[..] m () = << 7 >> end
module MJ = struct $let[..] m () = << 8 >> end

module N1 = F$(MI)
module N2 = F$(MJ)
module Za = N1.P (struct let d = 200 end)
module Zb = N2.P (struct let d = 300 end)

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d\n"
    N1.w N2.w N1.Z0.pv Za.pv Zb.pv
    $( Za.App1.gm () ) $( Zb.App2.gq () )
    $( N1.fm () ) $( N2.fm () )
