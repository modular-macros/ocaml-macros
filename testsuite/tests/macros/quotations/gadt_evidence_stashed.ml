(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Code that uses an equation, kept in a reference and returned out
   of the arm of the match that learned the equation *)

type (_, _) eql = Refl : ('a, 'a) eql

let s : string =
  $(let stash : type a b. (a, b) eql expr -> a expr -> b expr =
      fun r c ->
        let cell : b expr option ref = ref None in
        let _ : unit expr =
          << match $r with
             | Refl -> $(cell := Some << ($c : b) >>; << () >>) >> in
        Option.get !cell
    in
    stash (<< assert false >> : (int, string) eql expr) << 1 >>)
