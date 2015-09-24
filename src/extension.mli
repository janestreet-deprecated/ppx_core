open Asttypes
open Parsetree

type ('context, 'payload) t
(** Type of declared extensions.

    The ['context] type parameter describes where the extension is expected and the
    ['payload] one what its payload should contain. *)

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

val declare
  :  string
  -> 'context Context.t
  -> (payload, 'a, 'b) Ast_pattern.t
  -> 'a
  -> ('context, 'b) t

val convert : (_, 'a) t list -> extension -> 'a option

val check_unused : Ast_traverse.iter
