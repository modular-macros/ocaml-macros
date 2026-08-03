(* TEST
 readonly_files = "stdlibx.ml";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 all_modules = "stdlibx.ml stdlib_prefix_bypass.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Cross-stage level checking used to be bypassed for any USER unit
   whose name begins with "Stdlib" -- the exemption for the standard
   library matched "[intf]Stdlib" as a prefix of the printed Uid, which
   Stdlibx also satisfies.  The exempt set is now matched structurally
   on the compilation unit ("Stdlib" and "Stdlib__*"), so this level
   mismatch is reported like any other. *)

macro m () = let _ = Stdlibx.v in << 1 >>
