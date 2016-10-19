
type constant =
    Pconst_integer of string * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of char
  (* 'c' *)
  | Pconst_string of string * string option
  (* "constant"
     {delim|other constant|delim}
  *)
  | Pconst_float of string * char option
  (* 3.4 2e5 1.4e-4

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes are rejected by the typechecker.
  *)

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type arg_label =
    Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string (* ?label:T -> ... *)

type location = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type 'a loc = {
  txt : 'a;
  loc : location;
}

type lid =
  Lident of string
| Ldot of lid * string
| Lapply of lid * lid

(* 'a = attribute
   'b = core_type
   'c = module_expr
   'd = class_structure
*)
type ('a, 'b, 'c, 'd) pattern =
  {
    ppat_desc: ('a, 'b, 'c, 'd) pattern_desc;
    ppat_loc: location;
    ppat_attributes: 'a list;
  }

and ('a, 'b, 'c, 'd) pattern_desc =
  | Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of ('a, 'b, 'c, 'd) pattern * string loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of ('a, 'b, 'c, 'd) pattern list
  | Ppat_construct of lid loc * ('a, 'b, 'c, 'd) pattern option
  | Ppat_variant of label * ('a, 'b, 'c, 'd) pattern option
  | Ppat_record of
      (lid loc * ('a, 'b, 'c, 'd) pattern) list * closed_flag
  | Ppat_array of ('a, 'b, 'c, 'd) pattern list
  | Ppat_or of ('a, 'b, 'c, 'd) pattern * ('a, 'b, 'c, 'd) pattern
  | Ppat_constraint of ('a, 'b, 'c, 'd) pattern * 'b
  | Ppat_type of lid loc
  | Ppat_lazy of ('a, 'b, 'c, 'd) pattern
  | Ppat_unpack of string loc
  | Ppat_exception of ('a, 'b, 'c, 'd) pattern
  | Ppat_extension of 'a

and ('a, 'b, 'c, 'd) expression =
  {
    pexp_desc: ('a, 'b, 'c, 'd) expression_desc;
    pexp_loc: location;
    pexp_attributes: 'a list;
  }

and ('a, 'b, 'c, 'd) expression_desc =
  | Pexp_ident of lid loc
  | Pexp_constant of constant
  | Pexp_let of
      rec_flag * ('a, 'b, 'c, 'd) value_binding list
      * ('a, 'b, 'c, 'd) expression
  | Pexp_function of ('a, 'b, 'c, 'd) case list
  | Pexp_fun of
      arg_label * ('a, 'b, 'c, 'd) expression option
      * ('a, 'b, 'c, 'd) pattern * ('a, 'b, 'c, 'd) expression
  | Pexp_apply of
      ('a, 'b, 'c, 'd) expression * (arg_label * ('a, 'b, 'c, 'd) expression) list
  | Pexp_match of
      ('a, 'b, 'c, 'd) expression * ('a, 'b, 'c, 'd) case list
  | Pexp_try of ('a, 'b, 'c, 'd) expression * ('a, 'b, 'c, 'd) case list
  | Pexp_tuple of ('a, 'b, 'c, 'd) expression list
  | Pexp_construct of lid loc * ('a, 'b, 'c, 'd) expression option
  | Pexp_variant of label * ('a, 'b, 'c, 'd) expression option
  | Pexp_record of
      (lid loc * ('a, 'b, 'c, 'd) expression) list
      * ('a, 'b, 'c, 'd) expression option
  | Pexp_field of ('a, 'b, 'c, 'd) expression * lid loc
  | Pexp_setfield of
      ('a, 'b, 'c, 'd) expression * lid loc
      * ('a, 'b, 'c, 'd) expression
  | Pexp_array of ('a, 'b, 'c, 'd) expression list
  | Pexp_ifthenelse of
      ('a, 'b, 'c, 'd) expression * ('a, 'b, 'c, 'd) expression
      * ('a, 'b, 'c, 'd) expression option
  | Pexp_sequence of ('a, 'b, 'c, 'd) expression * ('a, 'b, 'c, 'd) expression
  | Pexp_while of ('a, 'b, 'c, 'd) expression * ('a, 'b, 'c, 'd) expression
  | Pexp_for of
      ('a, 'b, 'c, 'd) pattern * ('a, 'b, 'c, 'd) expression
      * ('a, 'b, 'c, 'd) expression * direction_flag
      * ('a, 'b, 'c, 'd) expression
  | Pexp_constraint of ('a, 'b, 'c, 'd) expression * 'b
  | Pexp_coerce of ('a, 'b, 'c, 'd) expression * 'b option * 'b
  | Pexp_send of ('a, 'b, 'c, 'd) expression * string
  | Pexp_new of lid loc
  | Pexp_setinstvar of string loc * ('a, 'b, 'c, 'd) expression
  | Pexp_override of (string loc * ('a, 'b, 'c, 'd) expression) list
  | Pexp_letmodule of string loc * 'c * ('a, 'b, 'c, 'd) expression
  | Pexp_letexception of extension_constructor * ('a, 'b, 'c, 'd) expression
  | Pexp_assert of ('a, 'b, 'c, 'd) expression
  | Pexp_lazy of ('a, 'b, 'c, 'd) expression
  | Pexp_poly of ('a, 'b, 'c, 'd) expression * 'b option
  | Pexp_object of 'd
  | Pexp_newtype of string * ('a, 'b, 'c, 'd) expression
  | Pexp_pack of 'c
  | Pexp_open of override_flag * lid loc * ('a, 'b, 'c, 'd) expression
  | Pexp_quote of ('a, 'b, 'c, 'd) expression
  | Pexp_escape of ('a, 'b, 'c, 'd) expression
  | Pexp_extension of 'a
  | Pexp_unreachable

and ('a, 'b, 'c, 'd) case =
  {
    pc_lhs: ('a, 'b, 'c, 'd) pattern;
    pc_guard: ('a, 'b, 'c, 'd) expression option;
    pc_rhs: ('a, 'b, 'c, 'd) expression;
  }

and ('a, 'b, 'c, 'd) value_binding =
  {
    pvb_pat: ('a, 'b, 'c, 'd) pattern;
    pvb_expr: ('a, 'b, 'c, 'd) expression;
    pvb_attributes: 'a list;
    pvb_loc: location;
  }
