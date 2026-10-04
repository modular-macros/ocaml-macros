(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Code that uses an equation learned by the pattern of a parameter, kept in a
   reference and returned out of its scope *)

type (_, _) eql = Refl : ('a, 'a) eql

let s : string =
  $(let stash : type a b. (a, b) eql expr -> a expr -> b expr =
      fun r c ->
        let cell : b expr option ref = ref None in
        let _ : unit expr =
          << (fun (Refl : (a, b) eql) -> $(cell := Some << ($c : b) >>; << () >>)) $r >> in
        Option.get !cell
    in
    stash (<< assert false >> : (int, string) eql expr) << 1 >>)
