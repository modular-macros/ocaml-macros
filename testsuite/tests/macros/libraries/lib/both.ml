let v = 10
$let s = 5
$let[..] add x = << $x + v >>
module Sub = struct
  let w = 20
  $let t = 6
end
module type V = sig val v : int end
module F (X : V) = struct let w = X.v + 1 end
