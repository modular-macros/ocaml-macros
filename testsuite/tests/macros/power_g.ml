(* TEST
   flags = "-g";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* As power.ml, under -g: a recursive macro's stage -1 translation must be a
   bare function, with no debug event wrapped around it. *)

macro rec pow x n =
  if n = 0 then <<1>>
  else << $x * $(pow x (n-1)) >>

let pow5 x = $(pow <<x>> 5)

let () = Printf.printf "%d\n" (pow5 2)
