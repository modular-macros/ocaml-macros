static x = 42
;;

let y = succ x (* Error: phase mismatch *)
;;

static () = Printf.printf "%d\n" x (* Error: this module is at phase 0 *)
;;

static () = ^Printf.printf "%d\n" x
;;

let () = Printf.printf "%d\n" $(Expr.of_int x)
;;
