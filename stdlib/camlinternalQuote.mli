module Identifier : sig
  type t

  type set_of_t = Set.Make(CamlinternalLam.Ident).t
  val rename : t -> t

  val new_scope : t list -> (unit -> 'a * set_of_t) -> string -> 'a * set_of_t

  val empty : unit -> set_of_t
  val free_var : t -> set_of_t
  val merge_free_vars: set_of_t -> set_of_t -> set_of_t
  val check : set_of_t -> string -> unit 

  val scope_extrusion_check: (unit -> 'a * set_of_t) -> 'a * set_of_t

  end
