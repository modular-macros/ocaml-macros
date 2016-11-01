static x = << List.map Pervasives.pred [1;2;3] >>;;
static y = << Some 42 >>;;

module Pervasives = struct
  let pred = Pervasives.succ
end;;

type fake_option =
  | Blah
  | Some of int
;;

let l = $x;;
let b =
  match $y with
  | Blah -> false
  | Some x -> x = 42
;;
