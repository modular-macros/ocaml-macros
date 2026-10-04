(* TEST
 toplevel;
*)

(* An object in compile-time code is refused in the toplevel as it is
   in a compiled unit: in a static binding, in a generator and in a
   splice. *)

$let o = object method m = 1 end;;
$let[..] g () = let p = object method m = 2 end in Expr.int p#m;;
let z = $(let q = object method m = 3 end in Expr.int q#m);;

(* Run-time code makes objects as ever, and may splice into them. *)
$let k : int = 4;;
let r = object method m = $(Expr.int k) end;;
r#m;;
