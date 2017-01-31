module Parsetree : sig

  module Loc : sig

    type t

    val none : t

    val unmarshal : string -> t

  end

  module Name : sig

    type t

    val mk : string -> Loc.t -> t

    val unmarshal : string -> t

  end

  module Var : sig

    type t

    val name : t -> Name.t

  end

  module Constant : sig

    type t

    val unmarshal : string -> t

    val integer : int -> t

    val floating : float -> t

    val char : char -> t

    val string : string -> t

  end

  module Ident : sig

    type t

    val unmarshal : string -> t

    (** [lfrommacro lid i] returns [Lfrommacro (lid, i)] *)
    val lfrommacro : t -> string -> int -> t

    (** Prints to stderr *)
    val eprint : t -> unit

  end

  module Label : sig

    type t

    val none : t

    val of_string : string -> t

  end

  module ArgLabel : sig

    type t

    val unmarshal : string -> t

  end

  module Variant : sig

    type t

    val of_string : string -> t

  end

  module Method : sig

    type t

    val of_string : string -> t

  end

  module Pat : sig

    type t

    val any : Loc.t -> t

    val var : Loc.t -> Var.t -> t

    val alias : Loc.t -> t -> Var.t -> t

    val constant : Loc.t -> Constant.t -> t

    val interval : Loc.t -> Constant.t -> Constant.t -> t

    val tuple : Loc.t -> t list -> t

    val construct : Loc.t -> Ident.t -> t option -> t

    val variant : Loc.t -> Variant.t -> t option -> t

    val record : Loc.t -> (Ident.t * t) list -> bool -> t

    val array : Loc.t -> t list -> t

    val or_ : Loc.t -> t -> t -> t

    val type_ : Loc.t -> Ident.t -> t

    val lazy_ : Loc.t -> t -> t

    val exception_ : Loc.t -> t -> t

  end

  module rec Case : sig

    type t

    val nonbinding : Loc.t -> Pat.t -> Exp.t -> t

    val simple : Loc.t -> Name.t -> (Var.t -> Exp.t) -> t

    val pattern : Loc.t -> Name.t list -> (Var.t list -> Pat.t * Exp.t) -> t

    val guarded :
      Loc.t -> Name.t list -> (Var.t list -> Pat.t * Exp.t * Exp.t) -> t

  end

  and Exp : sig

    type t

    val var : Loc.t -> Var.t -> t

    val ident : Loc.t -> Ident.t -> t

    val constant : Loc.t -> Constant.t -> t

    val local : Loc.t -> Name.t -> (Var.t -> t) ->  t

    val let_nonbinding : Loc.t -> Pat.t -> t -> t -> t

    val let_simple : Loc.t -> Name.t -> t -> (Var.t -> t) -> t

    val let_rec_simple : Loc.t -> Name.t list -> (Var.t list -> t list * t) -> t

    val let_pattern : Loc.t -> Name.t list -> t -> (Var.t list -> Pat.t * t) -> t

    val fun_nonbinding : Loc.t -> ArgLabel.t -> Pat.t -> t -> t

    val fun_simple : Loc.t -> Name.t -> ArgLabel.t -> t option -> (Var.t -> t) -> t

    val fun_pattern :
      Loc.t -> Name.t list -> ArgLabel.t -> t option ->
      (Var.t list -> Pat.t * t) -> t

    val function_ : Loc.t -> Case.t list -> t

    val apply : Loc.t -> t -> (ArgLabel.t * t) list -> t

    val match_ : Loc.t -> t -> Case.t list -> t

    val try_ : Loc.t -> t -> Case.t list -> t

    val tuple : Loc.t -> t list -> t

    val construct : Loc.t -> Ident.t -> t option -> t

    val variant : Loc.t -> Variant.t -> t option -> t

    val record : Loc.t -> (Ident.t * t) list -> t option -> t

    val field : Loc.t -> t -> Ident.t -> t

    val setfield : Loc.t -> t -> Ident.t -> t -> t

    val array : Loc.t -> t list -> t

    val ifthenelse : Loc.t -> t -> t -> t option -> t

    val sequence : Loc.t -> t -> t -> t

    val while_ : Loc.t -> t -> t -> t

    val for_ : Loc.t -> Name.t -> t -> t -> bool -> (Var.t -> t) -> t

    val send : Loc.t -> t -> Method.t -> t

    val assert_ : Loc.t -> t -> t

    val lazy_ : Loc.t -> t -> t

    val open_ : Loc.t -> bool -> Ident.t -> t -> t

    val quote : Loc.t -> t -> t

    val escape : Loc.t -> t -> t

    module Closed : sig

      type t

    end

    val to_closed : t -> Closed.t

    val of_closed : Closed.t -> t

  end

end

module Lambda : sig

  open CamlinternalLambda

  module Loc : sig

    type t

    val unmarshal : string -> t

  end

  module Constant : sig

    type t

    val unmarshal : string -> t

  end

  module Identifier : sig

    type t

    val mk : string -> t

    val unmarshal : string -> t

  end

  module Attribute : sig

    val unmarshal_inline_attr : string -> inline_attribute

    val unmarshal_specialise_attr : string -> specialise_attribute

    val unmarshal_function_kind : string -> function_kind

    val unmarshal_function_attr : string -> function_attribute

    val unmarshal_let_kind : string -> let_kind

    val unmarshal_value_kind : string -> value_kind

    val unmarshal_direction_flag : string -> direction_flag

    val unmarshal_method_kind : string -> meth_kind

  end

  module Primitive : sig

    val unmarshal : string -> primitive

  end

  module Exp : sig

    type t

    val var : Identifier.t -> t

    val constant : Constant.t -> t

    val application : Loc.t -> t -> t list -> bool
      -> inline_attribute -> specialise_attribute -> t

    val function_ : Loc.t -> function_kind -> Identifier.t list ->
      t -> function_attribute -> t

    val let_ : let_kind -> value_kind -> Identifier.t -> t -> t -> t

    val letrec : (Identifier.t * t) list -> t -> t

    val primitive : Loc.t -> primitive -> t list -> t

    val switch : t -> numconsts:int -> consts:(int * lambda) list
      -> numblocks:int -> blocks:(int * lambda) list
      -> failaction:lambda option
      -> t

    val stringswitch : Loc.t -> t -> (string * t) list -> t option -> t

    val staticraise : int -> t list -> t

    val staticcatch : t -> (int * Identifier.t list) -> t -> t

    val trywith : t -> Identifier.t -> t -> t

    val ifthenelse : t -> t -> t -> t

    val sequence : t -> t -> t

    val while_ : t -> t -> t

    val for_ : Identifier.t -> t -> t -> direction_flag -> t -> t

    val assign : Identifier.t -> t -> t

    val send : meth_kind -> t -> t -> t list -> Loc.t -> t

  end

end

val lambda_to_expr : Lambda.Exp.t -> 'a expr
