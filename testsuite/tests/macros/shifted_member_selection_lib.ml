let v = 5
$gen sm e = << $e * 2 >>
module Inner = struct
  let iv = 30
  $gen im e = << $e + 100 >>
end
