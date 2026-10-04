val a : int
$val x : int
val b : int

module Sub : sig
  val d : int
end

module Statics : sig
  $val y : int
  val e : int
end

module Restricted : sig
  val v : int
end

module Ints : Set.S with type elt = int

exception E

val h : int
val f : int -> int
