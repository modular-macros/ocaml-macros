(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The code is spliced under a second match on the same witness.  The
   evidence of the second arm is not that of the first. *)

type (_, _) eql = Refl : ('a, 'a) eql

let n : int =
  $(let restash : type a b. (a, b) eql expr -> a expr -> b expr =
      fun r c ->
        let cell : b expr option ref = ref None in
        let _ : unit expr =
          << match $r with
             | Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
        << match $r with Refl -> $(Option.get !cell) >>
    in
    restash << Refl >> << 1 >>)
