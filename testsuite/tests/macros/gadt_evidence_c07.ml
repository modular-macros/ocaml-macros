(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* C07: code using the equation, stashed in a reference and returned
   out of the arm.  The type checker admits it -- the cell is at the
   right type -- and the generator is stopped: the stashed code carries
   the arm's evidence as a free variable. *)

type (_, _) eql = Refl : ('a, 'a) eql

$gen c07 : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell : b expr option ref = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    Option.get !cell

let s : string = $(c07 (<< assert false >> : (int, string) eql expr) << 1 >>)
