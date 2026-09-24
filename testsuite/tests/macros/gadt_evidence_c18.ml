(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* C18: the code is spliced under a SECOND match on the same witness.
   Sound, but the evidence is checked by identity: the second arm's
   binder is not the first's. *)

type (_, _) eql = Refl : ('a, 'a) eql

$gen c18 : type a b. (a, b) eql expr -> a expr -> b expr =
  fun r c ->
    let cell : b expr option ref = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
    << match $r with Refl -> $(Option.get !cell) >>

let n : int = $(c18 << Refl >> << 1 >>)
