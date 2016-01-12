open Asttypes
open Parsetree

module Context : sig
  type 'a t

  val class_expr       : class_expr       t
  val class_field      : class_field      t
  val class_type       : class_type       t
  val class_type_field : class_type_field t
  val core_type        : core_type        t
  val expression       : expression       t
  val module_expr      : module_expr      t
  val module_type      : module_type      t
  val pattern          : pattern          t
  val signature_item   : signature_item   t
  val structure_item   : structure_item   t
end

module V2 : sig
  type t
  (** Type of declared extensions. *)

  (** [declare name context pattern expander] declares the extension names [name] for
      [context].

      [expander] is responsible for producing the code to replace the extension in the
      AST. It receives as argument:

      - [loc]: the location of the enclosing node. For instance for expression it is the
        [pexp_loc] field
      - [path]: the current module path
  *)
  val declare
    :  string
    -> 'context Context.t
    -> (payload, 'a, 'context) Ast_pattern.t
    -> (loc:Location.t -> path:string -> 'a)
    -> t

  (** Inline the result of the expansion into its parent. Only works for these contexts:

      - [class_field]
      - [class_type_field]
      - [signature_item]
      - [structure_item]
  *)
  val declare_inline
    :  string
    -> 'context Context.t
    -> (payload, 'a, 'context list) Ast_pattern.t
    -> (loc:Location.t -> path:string -> 'a)
    -> t

  class map_top_down : t list -> Ast_traverse.map_with_path
end

module Expert : sig
  (** This module allows to declare extensions that do not produce a value of the context
      type. This is typically useful for extensions point that depends on more things from
      the context than the path and location. *)

  type ('context, 'payload) t
  (** Type of declared expert extensions.

      The ['context] type parameter describes where the extension is expected and the
      ['payload] one what its payload should contain. *)

  val declare
    :  string
    -> 'context Context.t
    -> (payload, 'a, 'b) Ast_pattern.t
    -> 'a
    -> ('context, 'b) t

  val convert : (_, 'a) t list -> loc:Location.t -> extension -> 'a option
end

type ('context, 'payload) t = ('context, 'payload) Expert.t
  [@@deprecated "[since 2015-11] use Expert.t instead"]

val declare
  :  string
  -> 'context Context.t
  -> (payload, 'a, 'b) Ast_pattern.t
  -> 'a
  -> ('context, 'b) Expert.t
  [@@deprecated "[since 2015-11] use Expert.declare instead"]

val convert : (_, 'a) Expert.t list -> extension -> 'a option
  [@@deprecated "[since 2015-11] use Expert.convert instead"]

val check_unused : Ast_traverse.iter
