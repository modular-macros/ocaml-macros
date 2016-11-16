open Typedtree
open Lambda
open Asttypes

val quote_expression :
  (expression -> lambda) -> (Ident.t * int Env.PathMap.t) option ->
  expression -> lambda

val transl_close_expression : Location.t -> lambda -> lambda

(** Calls [CamlinternalQuote.Exp.local] to bind a fresh name to an identifier *)
val wrap_local : Location.t -> Ident.t -> string loc -> lambda -> lambda

val marshal_ident : Longident.t loc -> lambda

(** [transl_clos_field id i] returns the lambda code constructing
    [Lfrommacro (lid, i)], where [lid] is the contents of the variable referred
    to by [id] ([id] is assumed to be in scope). *)
val transl_clos_field : Location.t -> Ident.t -> int -> lambda
