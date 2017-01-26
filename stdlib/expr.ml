open ~CamlinternalQuote
open ~CamlinternalQuote.Lambda
open CamlinternalAST
open ~CamlinternalLambda
open ~Pervasives

macro of_bool =
  function
  | false -> <<false>>
  | true -> <<true>>

macro of_int i =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Pconst_integer (string_of_int i, None))) []

macro of_float f =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Pconst_float (string_of_float f, None))) []

macro of_char c =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Pconst_char c)) []

macro of_string s =
  lambda_to_expr @@
  Exp.constant @@ Constant.unmarshal @@ ~Marshal.to_string
    (Const_base (Pconst_string (s, None))) []

macro rec of_list f = function
  | [] -> << [] >>
  | x :: xs -> << $(f x) :: $(of_list f xs) >>

