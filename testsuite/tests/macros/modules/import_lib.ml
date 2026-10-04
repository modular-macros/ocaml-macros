module type S = sig val v : int end
$let run (module M : S) = M.v + 1
module Static = struct $let v = 50 end
$let make n : (module S) = (module struct let v = n end)
