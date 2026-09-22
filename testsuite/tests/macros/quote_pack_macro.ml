(* TEST
 readonly_files = "quote_pack_macro_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "quote_pack_macro_lib.ml quote_pack_macro.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "quote_pack_macro_lib.ml quote_pack_macro.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Packing a $gen-bearing module inside a quotation.  The quoted term is
   run-world code, so the pack must name the module's RUN-TIME block; the
   compile-time stage's rule -- a module reference denotes the module's
   M$macros block -- used to apply inside quotations too, so for a module
   of another unit the run-time object demanded
   Quote_pack_macro_lib$macros and the link failed with "No
   implementation provided".  (For a module of the unit being compiled
   the two addresses differ only in a persistent root, so that half
   happened to work.)  A let module or a let open cannot reach a
   $gen-bearing module at all -- typing refuses one there -- so the pack
   is the only route to this. *)

module type S = sig val v : int end

$gen packed_cross () = << (module Quote_pack_macro_lib : S) >>

let d = let module P = (val $(packed_cross ()) : S) in P.v

(* The same-unit half: M's run-time block holds 5, its compile-time block
   a dummy in that slot. *)
module M = struct
  let v = 7
  $gen two () = << 2 >>
end

$gen packed_here () = << (module M : S) >>

let e = let module P = (val $(packed_here ()) : S) in P.v

let () = Printf.printf "d = %d\ne = %d\n" d e
