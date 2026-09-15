(***************************************************************************)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


(** Abstract paths inside types or values.

    This module introduces paths within types and values. Path literals are
    written with the expression extension [%p]. The payload is parsed as an
    ordinary expression; the following forms are recognized as steps:

    - field: [field] or [path.field]
    - constructor: [Ctor] or [path.(Ctor)]
    - tuple element: [n/m] or [path.(n/m)] (literal ints)
    - list element: [[i]] or [path.[i]] (single-element list literal / index)
    - array element: [[|i|]] or [path.(i)] (single-element array literal / index)
    - optional type constraint on field/constructor: [(field : t)] or [(Ctor : t)]
    - root: [%p] with empty payload

    Examples:

    - [%p x.a]
    - [%p x.(A).(0/2)]
    - [%p [3]]
    - [%p (attributes : basic_commodity)]

*)

(** {2 Typed type paths} *)

type kind = [`Root|`Constructor|`Field|`List|`Array|`Tuple]

type ('a, 'b, +'c) t
(** Type paths. A value of type [('a,'b,'c) t] represents a path
    within the tree structure of the type ['a], reaching a
    sub-type ['b].  The third type argument ['c] is used to
    refine the kind of step on this path. *)

val extract: t:'a ttype -> ('a, 'b, _) t -> 'a -> 'b ttype * 'b
(** Extraction of sub-type and sub-value pointed to by a path.
    The extraction can fail (with a Failure exception) if
    the path is not a proper path in the specific value
    (constructors or list/array indices are wrong). *)

val patch: t: 'a ttype -> ('a, 'b, _) t -> 'a -> ('b -> 'b) -> 'a
(** Modify a sub value pointed by a path (the intermediate values are
    copied). If a (sub)value does not contain the given path, it is returned
    unmodified. Paths traversing lists or arrays are not supported. *)


val has_shape: t:'a ttype -> ('a, 'b, _) t -> 'a -> bool
(** Check if a path is valid w.r.t. a given value.  This functions
    returns true if and only if [extract] does not fail on the
    same arguments. *)

val force_shape: t:'a ttype -> ('a, 'b, _) t -> 'a -> 'a option
(** Attempt to modify the value to make the path valid. The path
    should end with a constructor (either constant, or unary with unit
    argument) and the function will only change that final constructor
    from the input value.  Paths traversing lists or arrays are not
    supported. *)

val extract_type: t:'a ttype -> ('a, 'b, _) t -> 'b ttype
(** Extraction of sub-type pointed to by a path. *)


(** {2 Composition, decomposition} *)

type ('a, 'b) composed = ('a, 'b, kind) t

val ( ^^ ): ('a, 'b, _) t -> ('b, 'c, _) t -> ('a, 'c) composed
(** Composition of type paths. *)

val is_prefix: ('a, 'b, _) t -> ('a, 'c, _) t -> ('b, 'c, kind) t option
(** [is_prefix path1 path2] checks if [path2] starts with [path1].
    When this is the case, the function returns the remaining path. *)

(** {2 Empty path} *)


type ('a, 'b) root = ('a, 'b, [`Root]) t

val root: ('a, 'a) root
[@@ocaml.deprecated "Use (.) instead"]
(** Root type path. *)

val is_empty: ('a, 'b, 'c) t -> ('a, 'b) Mlfi_types.TypEq.t option
(** Check if a type path is empty.  When this is the case,
    one knows statically that the source and target of the path
    are the same type, and this function returns a witness of
    this equality. *)


(** {2 Constructors} *)

type ('a, 'b) constructor = ('a, 'b, [`Constructor]) t
(** Constructors. Introduced with the syntax [.Constructor] or
    [.(Constructor)].  By convention, for a constant
    constructor, ['b] is equal to [unit].  For an n-ary
    constructor with n > 1, ['b] is the tuple of the types of
    the parameters of the constructor.  *)

val constructor_name: ('a, 'b) constructor -> string
val apply_constructor: t:'a ttype -> ('a, 'b) constructor -> 'b -> 'a
(** Build a value of a sum type by applying one of its constructors
    to the expected argument of this constructor. *)

val constructor_info: t:'a ttype -> ('a, 'b) constructor -> string * Mlfi_types.stype_properties * 'b ttype
(** For a given constructor in a sum type, returns its name, the associated type properties, and the type of its argument(s). *)

(** {2 Record fields} *)

type ('a, 'b) field = ('a, 'b, [`Field]) t
(** Record fields. Introduced with the syntax [.field_name] or [.(field_name)]. *)

val field_name: ('a, 'b) field -> string
val extract_field: t:'a ttype -> ('a, 'b) field -> 'a -> 'b
(** Extraction of a record field from a record value. *)

val set_field: t:'a ttype -> ('a, 'b) field -> 'a -> 'b -> 'a
(** Change the value of a record field (after copying the record value). *)

val extract_field_info: t:'a Mlfi_types.ttype -> ('a, 'b) field -> string * Mlfi_types.stype_properties * 'b Mlfi_types.ttype

val extract_field_type: t:'a ttype -> ('a, 'b) field -> 'b ttype
(** Same as [extract_type], but specialized to record fields, and more efficient. *)

(** {2 Lists} *)

type ('a, 'b) list_step = ('a, 'b, [`List]) t

val type_path_get_list_nth: int -> ('a list, 'a) list_step
(** List element. Introduced with the syntax [.[<index>]] or [.([<index>])]. *)

val is_list_prefix: ('a list, 'b, _) t -> (int * ('a, 'b, kind) t) option
(** A path starting from type ['a list] is either empty or it
    starts with a list element step on index [i].  This function
    discriminates between these two cases, by returning either
    [None] (empty path) or [Some (i, rest)] where [rest] is what
    remains in the path after the first step. *)

(** {2 Arrays} *)

type ('a, 'b) array_step = ('a, 'b, [`Array]) t

val type_path_get_array_nth: int -> ('a array, 'a) array_step
(** Array element type paths. Introduced with the syntax [.[|<element>|]] or [.([|<element>|])]. *)

(** {2 Tuples} *)

type ('a, 'b) tuple = ('a, 'b, [`Tuple]) t
(** Tuple type paths. Introduced with the syntax [. <element> / <arity>] or [.(<element>/<arity>)]. *)

(** {2 Rooted path} *)

type 'a rooted_path = TypePath: ('a, 'b, 'c) t -> 'a rooted_path
(** A type path with a known root type and unknown target and kind. *)

val prefix_rooted_path: ('a, 'b, _) t -> 'b rooted_path -> 'a rooted_path
(** Add a path prefix in front of a rooted path, producing a new
    rooted path with a different root. *)

val rooted_root: 'a rooted_path


(** {2 Internal representation} *)

module Internal:
sig
  type step =
    | Field of string (** Record field name *)
    | Constructor of string * int (** Constructor name and number of parameters *)
    | Tuple_nth of int (** Nth element of a tuple, 0-based index *)
    | List_nth of int (** Nth element of a list, 0-based index *)
    | Array_nth of int (** Nth element of an array, 0-based index *)

  val print_step: Format.formatter -> step -> unit
  val print_steps: Format.formatter -> step list -> unit
end  (** Internal untyped representation of paths. *)

val steps_of_path: ('a, 'b, _) t -> Internal.step list


val composed: ('a, 'b, _) t -> ('a, 'b) composed
