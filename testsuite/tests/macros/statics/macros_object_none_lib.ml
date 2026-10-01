$let s = 1
$let[..] g () = Expr.int s
let r = $(g ())
