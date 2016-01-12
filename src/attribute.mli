(** Attribute hygiene *)

(** This module provides hygiene for attributes. The goal is to report misuses of
    attributes to the user as soon as possible so that no mistyped attribute get silently
    ignored. *)

open Asttypes
open Parsetree

type ('context, 'payload) t
(** Type of declared attribute.

    The ['context] type parameter describes where the attribute is expected and the
    ['payload] one what its payload should contain. *)

type packed = T : (_, _) t -> packed

module Context : sig
  type 'a t

  val label_declaration       : label_declaration       t
  val constructor_declaration : constructor_declaration t
  val type_declaration        : type_declaration        t
  val type_extension          : type_extension          t
  val extension_constructor   : extension_constructor   t
  val pattern                 : pattern                 t
  val core_type               : core_type               t
  val expression              : expression              t
  val value_description       : value_description       t
  val class_type              : class_type              t
  val class_type_field        : class_type_field        t
  val class_infos             : _ class_infos           t
  val class_expr              : class_expr              t
  val class_field             : class_field             t
  val module_type             : module_type             t
  val module_declaration      : module_declaration      t
  val module_type_declaration : module_type_declaration t
  val open_description        : open_description        t
  val include_infos           : _ include_infos         t
  val module_expr             : module_expr             t
  val value_binding           : value_binding           t
  val module_binding          : module_binding          t
  val pstr_eval               : structure_item          t
  val pstr_extension          : structure_item          t
  val psig_extension          : signature_item          t
end

(** [declare fully_qualified_name context payload_pattern k] declares an attribute. [k] is
    used to build the value resulting from parsing the payload.

    For instance if a rewriter named "foo" expect the attribute [@@default] on record
    field declaration with an expression as payload:

    {[
      let default =
        Attribute.declare "foo.default"
          Attribute.Context.label_declaration
          Ast_pattern.(pstr (pstr_eval __ nil))
          (fun x -> x)
      ;;
    ]}

    [fully_qualified_name] is expected to be a dot-separated list of names. When matching,
    any full suffix will be accepted.  So for instance an attribute declared with name
    "foo.bar.default" will match exactly these attribute names: "default", "bar.default"
    and "foo.bar.default".

    When matching against a list of attributes on an item, if several matches are
    possible, the longest one is used. For instance using the attribute "foo.default"
    declared in the previous example, on this code it will match the [@foo.default 0]
    attribute:

    {[
      type t =
        { x : int [@default 42] [@foo.default 0]
        }
    ]}

    This is to allow the user to specify a [@default] attribute for all re-writers that
    use it but still put a specific one for one specific re-writer.


    It is not allowed to declare an attribute with a name that matches a
    previously-defined one on the same context. For instance trying to declare the same
    attribute twice will fail.
*)
val declare
  :  string
  -> 'a Context.t
  -> (payload, 'b, 'c) Ast_pattern.t
  -> 'b
  -> ('a, 'c) t

(** [reserve_namespace "foo"] has two implications:
      - one can't then declare an attribute inside this namespace
      - attributes within this namespace won't be reported by [check_unused]

    This is here to insure that the rewriter cohabits well with other rewriter
    or tools (e.g. merlin) which might leave attribute on the AST.

    N.B. the "merlin" namespace is reserved by default. *)
val reserve_namespace : string -> unit

val get : ('a, 'b) t -> 'a -> 'b option

(** [consume t x] returns the value associated to attribute [t] on [x] if present as well
    as [x] with [t] removed. *)
val consume : ('a, 'b) t -> 'a -> ('a * 'b) option

(** [remove_seen x attrs] removes the set of attributes matched by elements of
    [attrs]. Only remove them if they where seen by {!get} or {!consume}. *)
val remove_seen : 'a Context.t -> packed list -> 'a -> 'a

module Floating : sig
  type ('context, 'payload) t

  module Context : sig
    type 'a t

    val structure_item   : structure_item   t
    val signature_item   : signature_item   t
    val class_field      : class_field      t
    val class_type_field : class_type_field t
  end

  val declare
    :  string
    -> 'a Context.t
    -> (Parsetree.payload, 'b, 'c) Ast_pattern.t
    -> 'b
    -> ('a, 'c) t

  val convert : ('a, 'b) t list -> 'a -> 'b option
end

(** Code that is voluntarily dropped by a rewriter needs to be given to this object. All
    attributes inside will be marked as handled.
*)
val explicitly_drop : Ast_traverse.iter

(** Raise if there are unused attributes *)
val check_unused : Ast_traverse.iter

(** Replace all attribute names by a [String.copy] and collect them. To be used in
    conjuction with {!check_all_seen}. *)
val freshen_and_collect : Ast_traverse.map

(** Check that all attributes collected by {!freshen_and_collect} have been:

    - matched at least once by one of: {!get}, {!consume} or {!Floating.convert}
    - seen by [check_unused] (to allow white-listed attributed to pass through)

    This helps with faulty ppx rewriters that silently drop attributes.
*)
val check_all_seen : unit -> unit

(** Mark an attribute as seen and handled. This is only to make ppx rewriters that don't
    use ppx_core works well with ppx_driver. *)
val mark_as_handled_manually : attribute -> unit

(** Return the list of attributes that have been dropped so far: attributes that haven't
    been marked and are not present in the given AST. This is used to debug extensions
    that drop attributes. *)
val dropped_so_far_structure : structure -> string Location.loc list
val dropped_so_far_signature : signature -> string Location.loc list

val pattern
  :  ('a, 'b) t
  -> ('a, 'c, 'd) Ast_pattern.t
  -> ('a, 'b option -> 'c, 'd) Ast_pattern.t
