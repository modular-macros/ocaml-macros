(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Building code expressions from values.

    This module lifts values into the code expressions that quotations
    build and splices consume: [Expr.int 3] is the same code value as
    [<< 3 >>], available where the integer is computed rather than
    written.  Primitive values are lifted directly; structured values
    are built from the code expressions of their components. *)

type +!'a t = 'a expr

val unit : unit -> unit expr
(** [unit ()] is a code expression evaluating to [()]. *)

val bool : bool -> bool expr
(** [bool b] is a code expression evaluating to [b]. *)

val char : char -> char expr
(** [char c] is a code expression evaluating to [c]. *)

val uchar : Uchar.t -> Uchar.t expr
(** [uchar u] is a code expression evaluating to [u]. *)

val int : int -> int expr
(** [int n] is a code expression evaluating to [n]. *)

val int32 : int32 -> int32 expr
(** [int32 n] is a code expression evaluating to [n]. *)

val int64 : int64 -> int64 expr
(** [int64 n] is a code expression evaluating to [n]. *)

val nativeint : nativeint -> nativeint expr
(** [nativeint n] is a code expression evaluating to [n]. *)

val float : float -> float expr
(** [float f] is a code expression evaluating to [f], exactly
    (infinities and negative zero included), except that a NaN keeps
    its sign but not its payload. *)

val string : string -> string expr
(** [string s] is a code expression evaluating to a string equal to
    [s]. *)

val complex : Complex.t -> Complex.t expr
(** [complex z] is a code expression evaluating to [z]. *)

val option : 'a expr option -> 'a option expr
(** [option o] is a code expression evaluating to [None] if [o] is
    [None], and to [Some v] with [v] the value of [e] if [o] is
    [Some e]. *)

val list : 'a expr list -> 'a list expr
(** [list l] is a code expression evaluating to the list of the values
    of [l]'s elements, in order. *)

val iarray : int -> (int -> 'a expr) -> 'a iarray expr
(** [iarray n f] is a code expression evaluating to an immutable array
    of length [n] whose element [i] holds the value of [f i].  [f] is
    applied to [0] ... [n - 1] in order when [iarray] itself runs; the
    element expressions are later evaluated in an unspecified order,
    as in an array literal.

    @raise Invalid_argument if [n] is negative. *)

val either : ('a expr, 'b expr) Either.t -> ('a, 'b) Either.t expr
(** [either e] is a code expression evaluating to [Left v] or
    [Right v], following [e]'s constructor. *)

val result : ('a expr, 'b expr) Result.t -> ('a, 'b) Result.t expr
(** [result r] is a code expression evaluating to [Ok v] or
    [Error v], following [r]'s constructor. *)

val pair : ('a expr * 'b expr) -> ('a * 'b) expr
(** [pair (e1, e2)] is a code expression evaluating to the pair of
    [e1]'s and [e2]'s values. *)

val func : ('a expr -> 'b expr) -> ('a -> 'b) expr
(** [func f] is a code expression evaluating to a function whose body
    is built by applying [f] to the code of the function's
    parameter. *)

val unsafe_lift_constant : 'a -> 'a expr
(** [unsafe_lift_constant v] is a code expression holding a copy of
    [v]'s runtime representation as a single constant, taken when
    [unsafe_lift_constant] runs -- later changes to [v] do not affect
    it.  The representation must be built from immediates, strings,
    boxed numbers, flat float blocks (float arrays, [floatarray]
    values, all-float records), and blocks of these; it must not be
    cyclic ([unsafe_lift_constant] does not terminate on cyclic
    input).

    Unsafe because the type ['a] is not consulted: on a value with
    mutable parts -- an array, a [ref], a [bytes], a mutable record
    field -- every evaluation of the expression shares the one
    constant rather than rebuilding it, so mutations flow between
    evaluations that a literal in the same position would keep
    separate.  Restrict use to values of immutable type.

    @raise Invalid_argument on a value whose representation constants
    cannot express: a function, an object, a lazy value, or a custom
    block that is not a boxed number. *)

(** {1 Dispatch and recursion}

    These build code that no quotation of fixed shape can express: a
    dispatch on a value among cases whose number is known only when
    the code is built, and a group of recursive functions whose
    members are added as the code is built.  Like quotations, they keep
    track of the variables that the code combined mentions, and make
    fresh binders. *)

module type immediate = sig type t [@@immediate] end
(** A type whose values are immediate: [char], [bool], [int], or a
    variant whose constructors take no arguments.  A value is
    dispatched on by the integer that represents it: the code of a
    character, the position of a constructor in its declaration. *)

val switch :
  (module I : immediate) -> I.t expr -> numconsts:int ->
  (I.t * 'a expr) list -> 'a expr option -> 'a expr
(** [switch (module I) e ~numconsts cases default] is a code expression
    that evaluates [e] and then the expression of the case whose key
    is [e]'s value, by a jump table of [numconsts] entries, at most
    65535, the largest that bytecode can index.  A value
    with no case, and a value whose integer is not between [0] and
    [numconsts - 1], evaluates [default]; with no [default], it raises
    [Invalid_argument "Expr.switch"].

    @raise Invalid_argument if [numconsts] is not positive or exceeds
    65535, or if a key is repeated or its integer is not between [0]
    and [numconsts - 1]. *)

val switch_intervals :
  (module I : immediate) -> I.t expr -> numconsts:int ->
  ((I.t * I.t) list * 'a expr) list -> 'a expr option -> 'a expr
(** As {!switch}, each case serving the values in a list of inclusive
    intervals.  The expression of a case is built once, however many
    values it serves, and equal expressions are shared.  With no
    [default], a value without a case raises
    [Invalid_argument "Expr.switch_intervals"].

    @raise Invalid_argument also on a case without intervals, and on
    intervals that are empty, out of range, or overlapping. *)

val string_switch : string expr -> (string * 'a expr) list -> 'a expr ->
  'a expr
(** [string_switch e cases default] is a code expression that
    evaluates [e] and then the expression of the case whose string is
    equal to [e]'s value, or [default] if there is none.  Equal
    expressions are shared.

    @raise Invalid_argument if a string is repeated. *)

(** Groups of recursive functions. *)
module Letrec : sig
  type group
  (** A group of recursive functions being built *)

  val group : (group -> 'a expr) -> 'a expr
  (** [group f] is the code expression [f g], in the scope of the
      functions bound in [g] while [f g] is built: a [let rec] of
      those functions around it.

      @raise Invalid_argument if a function of the group has no
      definition. *)

  val bind : group -> ('a expr -> 'a expr) -> 'a expr
  (** [bind g d] binds a new function in [g], whose definition is
      [d f], [f] being the code of the function, and returns [f].  [f]
      may be used in the definitions of [g] and in the expression of
      its [group], and nowhere else: a code value that mentions it
      outside them is a scope extrusion, as for a variable bound in a
      quotation.

      @raise Invalid_argument if [d f] is not a function
      ([<< fun x -> ... >>]), or if [bind] is called outside the
      building of [g]. *)
end
