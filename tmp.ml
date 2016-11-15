let x = 42
macro y = <<x>>

let () = Printf.printf "%d\n" $y
