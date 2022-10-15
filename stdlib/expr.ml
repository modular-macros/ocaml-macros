open ~CamlinternalQuote
open ~CamlinternalQuote.Lambda
open ~CamlinternalLambda
open ~Pervasives

let~ of_bool =
  function
  | false -> <<false>>
  | true -> <<true>>

let~ of_int i =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_int i)) []

let~ of_float f =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_float (string_of_float f))) []

let~ of_char c =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_char c)) []

let~ of_string s =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_string (s, None))) []

let~ rec of_list f = function
  | [] -> << [] >>
  | x :: xs -> << $(f x) :: $(of_list f xs) >>

