(* The aliased target: two macros and a run-time value, one macro
   capturing [w] so the environment must survive the alias too. *)
let w = 4
module Inner = struct
  let v = 5
  macro m () = << w + 5 >>
  macro k e = << $e * 3 >>
end
