module M : sig macro x : int expr end = struct
  static lol = 1334
  macro x = <<42>>
  let y = 42
end

let () = Printf.printf "%d\n" $M.x
