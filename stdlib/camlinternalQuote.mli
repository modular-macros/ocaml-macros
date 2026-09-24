module Identifier : sig
  type t

  type set_of_t

  type _ Effect.t += FreeVar : set_of_t -> unit Effect.t

  val rename : t -> t

  val new_scope : t list -> (unit -> 'a * set_of_t) -> 'a * set_of_t

  val empty : set_of_t
  val free_var : t -> set_of_t
  val merge_free_vars: set_of_t -> set_of_t -> set_of_t
  val check : set_of_t -> unit

  val names : set_of_t -> string list
end
