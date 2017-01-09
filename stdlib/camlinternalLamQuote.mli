module Loc : sig
  type t
  val none : t
  val unmarshal : string -> t
end

module Constant : sig
  type t
  val unmarshal : string -> t
end

module Lambda : sig
  val constant : Loc.t -> Constant.t -> t
end
