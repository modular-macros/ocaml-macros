let hidden = 0
$let x = print_endline "Import_lib"; Import_base.base + 1
let v = hidden + 1
module Sub = struct
  $let hidden = 5
  let u = v + 1
  $let y = x + hidden
end
module Plain = struct
  let t = 3
end
