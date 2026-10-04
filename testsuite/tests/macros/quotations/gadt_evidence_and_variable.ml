(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Code that uses both a variable and an equation outside their scopes *)

type (_, _) eql = Refl : ('a, 'a) eql

let s : string =
  $(let stash : type a b. (a, b) eql expr -> b expr =
      fun r ->
        let cell : b expr option ref = ref None in
        let _ : (a -> unit) expr =
          << fun (x : a) ->
               match $r with
               | Refl -> $(cell := Some << (x : b) >>; << () >>) >> in
        Option.get !cell
    in
    stash (<< assert false >> : (int, string) eql expr))
