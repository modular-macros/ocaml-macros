open ~CamlinternalQuote
open ~CamlinternalQuote.Lambda
open ~CamlinternalLambda
open ~Pervasives

macro of_bool =
  function
  | false -> <<false>>
  | true -> <<true>>

macro of_int i =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_int i)) []

macro of_float f =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_float (string_of_float f))) []

macro of_char c =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_char c)) []

macro of_string s =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Const_string (s, None))) []

macro rec of_list f = function
  | [] -> << [] >>
  | x :: xs -> << $(f x) :: $(of_list f xs) >>

