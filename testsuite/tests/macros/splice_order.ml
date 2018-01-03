macro one = << 1 >>;;
macro two = << 2 >>;;
macro three = << 3 >>;;
macro four = << 4 >>;;
macro five = << 5 >>;;

let () =
  Printf.printf "%d\n" $(one);
  Printf.printf "%d\n" $(two);
  Printf.printf "%d\n" $(three);
;;

let () =
  Printf.printf "%d\n" $(four);
;;

let () =
  Printf.printf "%d\n" $(five);
;;

static x = ~Pervasives.ref 0;;

let () =
  print_int $(let open ~Pervasives in x := 1; Expr.of_int !x)
;;

let () =
  print_int $(let open ~Pervasives in x := 2; Expr.of_int !x)
;;

let () =
  print_int $(let open ~Pervasives in x := 3; Expr.of_int !x)
;;
