open ~CamlinternalQuote
open ~Pervasives

macro of_bool =
  function
  | false -> <<false>>
  | true -> <<true>>

macro of_int i =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.integer i

macro of_float f =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.floating f

macro of_char c =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.char c

macro of_string s =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.string s

macro rec of_list f = function
  | [] -> << [] >>
  | x :: xs -> << $(f x) :: $(of_list f xs) >>

