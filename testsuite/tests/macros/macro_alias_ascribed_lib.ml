(* The aliased target: two macros and a run-time value, one $gen
   capturing [w] so the environment must survive the alias too. *)
let w = 4
module Inner = struct
  let v = 5
  $gen m () = << w + 5 >>
  $gen k e = << $e * 3 >>
end
