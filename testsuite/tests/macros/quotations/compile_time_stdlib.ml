(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The standard library serves compile-time code as well as run-time
   code. *)

let a = $(print_endline "a"; << 1 >>)
let b = $(if List.length [1; 2] > String.length "b" then << 1 >> else << 2 >>)
let c = $(if Stdlib.min 1 2 > Stdlib.List.length [] then << 1 >> else << 2 >>)
let d =
  $(let open List in if length (map succ [1]) > 0 then << 1 >> else << 2 >>)
let e = $(List.(if length [] > 0 then << 1 >> else << 2 >>))

(* Code quoted in compile-time code is run-time code. *)

let f = $(<< List.length [1; 2] >>)
let g = $(let n = << List.length [1; 2] >> in << List.init $n succ >>)

(* A name bound at no level is found whatever the level of its use. *)

let h = << (None : int List.t option) >>
let i = $(let l : int List.t = [] in ignore (Some l); << Either.Left 1 >>)
