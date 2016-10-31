static f () = << List.map Pervasives.pred [1;2;3] >>;;

module Pervasives = struct
  let pred = Pervasives.succ
end;;

let l = $(f ());;
