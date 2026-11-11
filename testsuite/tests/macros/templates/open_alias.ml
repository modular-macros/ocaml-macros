(* TEST
 readonly_files = "open_alias_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "open_alias_lib.ml open_alias.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "open_alias_lib.ml open_alias.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Opening and including an alias to a unit with compile-time
   components.  An open of a path binds nothing: what it names is
   reached by paths, through aliases to their targets.  An include binds
   the items of the unit it names. *)

module A = Open_alias_lib
module B = struct module L = Open_alias_lib end
module N = struct $let n : int = 4 end

open A
let x = $(Expr.int k)
module U = T$(N)

module C = struct
  include A
  let y = $(Expr.int k) + 1
end

open B
let z = $(Expr.int L.k)

let () = Printf.printf "%d %d %d %d\n" x U.v C.y z
