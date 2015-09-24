open Parsetree

module type Loc = sig
  val loc : Location.t
end

module type Additional_helpers = sig
  type 'a with_loc

  val eint       : (int       -> expression) with_loc
  val echar      : (char      -> expression) with_loc
  val estring    : (string    -> expression) with_loc
  val efloat     : (string    -> expression) with_loc
  val eint32     : (int32     -> expression) with_loc
  val eint64     : (int64     -> expression) with_loc
  val enativeint : (nativeint -> expression) with_loc
  val ebool      : (bool      -> expression) with_loc

  val pint       : (int       -> pattern) with_loc
  val pchar      : (char      -> pattern) with_loc
  val pstring    : (string    -> pattern) with_loc
  val pfloat     : (string    -> pattern) with_loc
  val pint32     : (int32     -> pattern) with_loc
  val pint64     : (int64     -> pattern) with_loc
  val pnativeint : (nativeint -> pattern) with_loc
  val pbool      : (bool      -> pattern) with_loc

  val eunit : expression with_loc
  val punit : pattern    with_loc

  (** [evar id] produces a [Pexp_ident _] expression, it parses its input so you can pass
      any dot-separated identifier, for instance: [evar ~loc "Foo.bar"]. *)
  val evar : (string -> expression) with_loc
  val pvar : (string -> pattern   ) with_loc

  (** Same as pexp_apply but without labels *)
  val eapply : (expression -> expression list -> expression) with_loc

  val eabstract : (pattern list -> expression -> expression) with_loc

  val pconstruct : constructor_declaration -> pattern    option -> pattern
  val econstruct : constructor_declaration -> expression option -> expression

  val elist : (expression list -> expression) with_loc
  val plist : (pattern    list -> pattern   ) with_loc

  val nonrec_type_declaration :
    (name:string Location.loc
     -> params:(core_type * Asttypes.variance) list
     -> cstrs:(core_type * core_type * Location.t) list
     -> kind:type_kind
     -> private_:Asttypes.private_flag
     -> manifest:core_type option
     -> type_declaration
    ) with_loc
end

module type Located = sig
  type 'a with_loc

  type 'a t = 'a Location.loc

  val mk : ('a -> 'a t) with_loc

  val map        : ('a -> 'b) -> 'a t -> 'b t
  val map_lident : string t -> Longident.t t

  val of_ident : (Ident.t -> string t) with_loc

  val lident : (string -> Longident.t t) with_loc

  val lident_of_ident : (Ident.t -> Longident.t t) with_loc
end

type 'a without_location = 'a
type 'a with_location    = loc:Location.t -> 'a

module type S = sig
  module Located : Located
    with type 'a with_loc := 'a without_location

  include module type of Ast_builder_generated.Make(struct let loc = Location.none end)

  include Additional_helpers
    with type 'a with_loc := 'a without_location
end
