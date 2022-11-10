open Typedtree
open Lambda
open Asttypes

val quote_expression :
  phase:int ->
  (expression -> lambda) -> (Ident.t * int Env.PathMap.t) option ->
  expression -> lambda

val transl_close_expression : Location.t -> lambda -> lambda

(** Calls [CamlinternalQuote.Exp.local] to bind a fresh name to an identifier *)
val wrap_local : Location.t -> Ident.t -> string loc -> lambda -> lambda

(** Quotes a path. *)
val path_arg : Location.t -> lambda -> lambda

(** [transl_clos_field id s i] returns the lambda code constructing
    [Lfrommacro (lid, s, i)], where [lid] is the contents of the variable
    referred to by [id] ([id] is assumed to be in scope). *)
val transl_clos_field : Location.t -> Ident.t -> string -> int -> lambda
