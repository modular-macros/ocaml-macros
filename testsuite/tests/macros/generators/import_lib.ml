(* The generators of this unit build code that mentions values that
   its interface hides. *)

let visible = 1
let hidden = 100

$let[..] add x = << $x + hidden >>
$let[..] add_twice x = add (add x)
$let[..] rec rec_add n x = if n = 0 then x else rec_add (n - 1) (add x)

module Sub = struct
  let factor = 7
  $let[..] mul x = << $x * factor + hidden >>
end

$let shift = 1000
