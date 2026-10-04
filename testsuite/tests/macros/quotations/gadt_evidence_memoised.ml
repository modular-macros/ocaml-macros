(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The code that leaves the arm uses the equation through an expansion
   that the arm made first: the use is recorded all the same. *)

type (_, _) eql = Refl : ('a, 'a) eql

let s : string =
  $(let stash : type a b. (a, b) eql expr -> a expr -> b expr =
      fun r c ->
        let cell : b expr option ref = ref None in
        let _ : unit expr =
          << match $r with
             | Refl ->
                 let _ : b = $c in
                 $(cell := Some << ($c : b) >>; << () >>) >> in
        Option.get !cell
    in
    stash (<< assert false >> : (int, string) eql expr) << 1 >>)
