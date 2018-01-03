macro one = << 1 >>;;
macro two = << 2 >>;;
macro three = << 3 >>;;

let () =
  Printf.printf "%d\n" $(one);
  Printf.printf "%d\n" $(two);
  Printf.printf "%d\n" $(three);
;;
