let v = 5
macro sm e = << $e * 2 >>
module Inner = struct
  let iv = 30
  macro im e = << $e + 100 >>
end
