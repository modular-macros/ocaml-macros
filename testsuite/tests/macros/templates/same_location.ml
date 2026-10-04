(* TEST
 readonly_files = "same_location_ppx.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/same_location_ppx.exe";
 all_modules = "same_location_ppx.ml";
 ocamlc.byte;
 program = "${test_build_directory}/same_location.byte";
 module = "";
 all_modules = "same_location.ml";
 flags = "-ppx ${test_build_directory}/same_location_ppx.exe";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Template applications that share one location, as a ppx may give
   them, are still applications of their own. *)

module F $(X : sig val v : int end) = struct let w = X.v * 10 end
module G $(X : sig val v : int end) = struct let w = X.v + 100 end
module A = struct include F$(struct let v = 1 end) end
module B = struct include G$(struct let v = 2 end) end
module H (Y : sig val w : int end) = struct let z = Y.w end
module C = H (F$(struct let v = 3 end))
module D = H (G$(struct let v = 4 end))
let () = Printf.printf "%d %d %d %d\n" A.w B.w C.z D.z
