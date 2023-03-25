let s = Printf.sprintf "%d %d %d" $(<<1>>) $(<<2>>) $(<<3>>);;
let () = assert (s = "1 2 3");;
