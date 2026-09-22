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
(** [float f] is a code expression evaluating to [f], exactly ([nan],
    infinities and negative zero included). *)

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
