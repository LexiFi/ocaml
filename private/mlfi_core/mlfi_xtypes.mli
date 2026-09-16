(***************************************************************************)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


(** Operations on ttypes. *)

(** {2 Construction of ttypes} *)

val option: 'a ttype -> 'a option ttype
val list: 'a ttype -> 'a list ttype
val array: 'a ttype -> 'a array ttype
val lazyt: 'a ttype -> 'a Lazy.t ttype
val pair: 'a ttype -> 'b ttype -> ('a * 'b) ttype
val triple: 'a ttype -> 'b ttype -> 'c ttype -> ('a * 'b * 'c) ttype
val quartet: 'a ttype -> 'b ttype -> 'c ttype -> 'd ttype -> ('a * 'b * 'c * 'd) ttype
val quintet: 'a ttype -> 'b ttype -> 'c ttype -> 'd ttype -> 'e ttype -> ('a * 'b * 'c * 'd * 'e) ttype
val arrow: ?label:string -> 'a ttype -> 'b ttype -> ('a -> 'b) ttype
val result: 'a ttype -> 'b ttype -> ('a, 'b) result ttype

val unlistt: 'a list ttype -> 'a ttype

val unlazyt: 'a Lazy.t ttype -> 'a ttype

(** {2 Safe inspection of ttypes} *)

type 'a record_builder

module RecordField : sig
  type ('s, 't) t
  (** Runtime representation of fields of type ['t] in records of type ['s].
      Also used to represent tuple components. *)

  val ttype: (_, 't) t -> 't ttype

  val name: _ t -> string
  (** Empty string for tuples. *)

  val props: _ t -> (string * string) list
  (** Empty list for tuples. *)

  val get: ('s, 't) t -> 's -> 't
  (** Extract the value corresponding to the given field of a record value. *)

  val set: ('s, 't) t -> 's record_builder -> 't -> unit
  (** To be used in the callback passed to [Record.make]. *)

  val path: ('s, 't) t -> ('s, 't) Mlfi_type_path.composed

  val field_path: ('s, 't) t -> ('s, 't) Mlfi_type_path.field
  (** Only for records, not tuples. *)
end

type 's has_record_field = Field: ('s, 't) RecordField.t -> 's has_record_field

exception Missing_field_in_record_builder

type 's field_builder = { mk: 't. ('s, 't) RecordField.t -> 't }
[@@ocaml.unboxed]

module Record: sig
  type 's t
  (** Runtime representation of records or tuples of type ['s]. *)

  val ttype: 's t -> 's ttype

  val fields: 's t -> 's has_record_field list

  val build: 's t -> 's field_builder -> 's
  (** Create a record value.  The polymorphic [mk] callback is guaranteed
      to be called in the same order as the [fields] list. *)

  val make: ?default:'s -> 's t -> ('s record_builder -> unit) -> 's
  (** Create a record value.  Contrary to [build], this function allows
      the client code to decide in which order fields are populated.

      If the default is not specified, the callback must call the
      [RecordField.set] function at least once on each field (passing
      the provided [record_builder]).  If the default is specified,
      it is used to populate fields which have not been set explicitly.
  *)

  val find_field: 's t -> string -> 's has_record_field option
  (** Only for records, not tuples. *)

  val find_field_typed: 's t -> ('s, 't) Mlfi_type_path.field -> ('s, 't) RecordField.t
  (** Only for records, not tuples. *)

  val path: 's t -> string

  type 's memo = ..
  val memo: 's t -> 's memo array
  val set_memo: 's t -> 's memo array -> unit
end

module Constructor : sig
  type ('s, 't) t
  (** Runtime representation of constructors of type ['t] in sum types of type ['s].
      The type of constructor is defined as follows:

      - A constructor without an argument has type "unit".
      - A constructor with multiple arguments has a tuple type.
      - A constructor with an inline record arguments has a pseudo-record type.
  *)

  val ttype: (_, 't) t -> 't ttype

  val index: _ t -> int

  val name: _ t -> string

  val props: _ t -> (string * string) list

  val project: ('s, 't) t -> 's -> 't option
  (** Check if a value of the sum type matches the given constructor;
      when this is the case, returns the constructor's argument. *)

  val project_exn: ('s, 't) t -> 's -> 't
  (** @raise Not_found if not the proper constructor.
      Faster than [project] when not in the error case. *)

  val inject:('s, 't) t -> 't -> 's
  (** Create a value of the sum type with the given constructor and
      its argument. *)

  val path: ('s, 't) t -> ('s, 't) Mlfi_type_path.constructor

  val nb_args: ('s, 't) t -> int

  type ('s, 't) memo = ..
  val memo: ('s, 't) t -> ('s, 't) memo array
  val set_memo: ('s, 't) t -> ('s, 't) memo array -> unit
end

type 's has_constructor = Constructor: ('s, 't) Constructor.t -> 's has_constructor

module Sum: sig
  type 's t

  val ttype: 's t -> 's ttype

  val path: 's t -> string

  val constructors: 's t -> 's has_constructor array

  val get_constructor_index: 's t -> 's -> int
  (** Find the index (in [constructors]) of the constructor
      corresponding to the given value of the sum type. *)

  val constructor: 's t -> 's -> 's has_constructor
  (** Extract the constructor corresponding to the given value of the
      sum type. *)

  val lookup_constructor: 's t -> string -> int
  (** Find the index of the constructor with the given name.  Returns
      -1 if not found.  The first call to this function is slow (it
      builds an optimized lookup table).  *)


  val is_enum: 's t -> bool
  (** Whether the sum type is an enumeration (only consisting of constant constructors). *)

  type 's memo = ..
  val memo: 's t -> 's memo array
  val set_memo: 's t -> 's memo array -> unit
end

module Method: sig
  type ('s, 't) t
  (** Runtime representation of methods of type ['t] in objects of
      type ['s]. *)

  val ttype: (_, 't) t -> 't ttype

  val name: _ t -> string

  val call: ('s, 't) t -> 's -> 't
end

type 's has_method = Method: ('s, 't) Method.t -> 's has_method


module Object: sig
  type 's t
  (** Runtime representation of objects of type ['t]. *)

  val ttype: 's t -> 's ttype

  val methods: 's t -> 's has_method list
  (** Sorted by method name. *)

  (* TODO: add a lookup function (by method name). *)
end

type _ is_function = Function: (string * 'b ttype * 'c ttype) -> ('b -> 'c) is_function
type _ is_list = List: 't ttype -> ('t list) is_list
type _ is_array = Array: 't ttype -> ('t array) is_array
type _ is_option = Option: 't ttype -> ('t option) is_option
type _ is_tuple2 = Tuple2: ('a ttype * 'b ttype) -> ('a * 'b) is_tuple2
type _ is_tuple3 = Tuple3: ('a ttype * 'b ttype * 'c ttype) -> ('a * 'b * 'c) is_tuple3
type _ is_lazy = Lazy: 't ttype -> ('t Lazy.t) is_lazy
type _ is_result = Result: ('a ttype * 'b ttype) -> ('a, 'b) Stdlib.result is_result


val is_list: 'a ttype -> 'a is_list option
val is_array: 'a ttype -> 'a is_array option
val is_option: 'a ttype -> 'a is_option option
val is_function: 'a ttype -> 'a is_function option
val is_tuple2: 'a ttype -> 'a is_tuple2 option
val is_tuple3: 'a ttype -> 'a is_tuple3 option
val is_lazy: 'a ttype -> 'a is_lazy option
val is_result: 'a ttype -> 'a is_result option

val is_record: 'a ttype -> 'a Record.t option
val is_tuple: 'a ttype -> 'a Record.t option
val is_sum: 'a ttype -> 'a Sum.t option
val is_object: 'a ttype -> 'a Object.t option
val is_prop: 'a ttype -> ((string * string) list * 'a ttype) option
val is_abstract: 'a ttype -> (string * 'a ttype * Mlfi_types.stype list) option

module type ABSTRACT_1 =
sig
  type 'a t
  val t: unit t ttype
end

module type ABSTRACT_1_MATCHER_SIG = sig
  type 'a t
  type _ is_t = Is: 'b ttype * ('a, 'b t) Mlfi_types.TypEq.t -> 'a is_t
  val is_t: 'a ttype -> 'a is_t option
end

module ABSTRACT_1_MATCHER (T : ABSTRACT_1) : sig
  include ABSTRACT_1_MATCHER_SIG with type 'a t = 'a T.t
  val name: string
end

module COMPOSE_ABSTRACT_1_MATCHER (T : ABSTRACT_1_MATCHER_SIG) (S : ABSTRACT_1_MATCHER_SIG) :
  ABSTRACT_1_MATCHER_SIG with type 'a t = 'a T.t S.t

val make_abstract: 'a ttype -> 'a ttype

type 'a xtype
  = Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | Option: 'b ttype * 'b xtype Lazy.t -> 'b option xtype
  | List: 'b ttype * 'b xtype Lazy.t -> 'b list xtype
  | Array: 'b ttype * 'b xtype Lazy.t  -> 'b array xtype
  | Floatarray : floatarray xtype
  | Function: (string * ('b ttype * 'b xtype Lazy.t) * ('c ttype * 'c xtype Lazy.t)) -> ('b -> 'c) xtype
  | Sum: 'a Sum.t -> 'a xtype
  | Tuple: 'a Record.t -> 'a xtype
  | Record: 'a Record.t -> 'a xtype
  | Lazy: ('b ttype * 'b xtype Lazy.t) -> 'b Lazy.t xtype
  | Prop: ((string * string) list * 'a ttype * 'a xtype Lazy.t) -> 'a xtype
  | Object: 'a Object.t -> 'a xtype
  | Abstract: (string * 'a ttype * Mlfi_types.stype list) -> 'a xtype

val xtype_of_ttype: 'a ttype -> 'a xtype
val ttype_of_xtype: 'a xtype -> 'a ttype

val xtype_of_constructor: (_, 'a) Constructor.t -> 'a xtype
val xtype_of_field: (_, 'a) RecordField.t -> 'a xtype
val xtype_of_method: (_, 'a) Method.t -> 'a xtype

val get_first_props_xtype: 'a xtype -> (string * string) list * 'a xtype

val get_first_props_ttype: 'a ttype -> (string * string) list * 'a ttype

val remove_first_props_xtype: 'a xtype -> 'a xtype

type sttype = Ttype: 'a ttype -> sttype

val sttype_of_stype: Mlfi_types.stype -> sttype

val all_paths: root:'root ttype -> target:'target ttype -> ('root, 'target, Mlfi_type_path.kind) Mlfi_type_path.t list
(** Returns all the paths leading to a value of type ['target] inside
    a value of type ['root]. Does not traverse list, array, lazy, objects.
    Will loop on recursive types.
*)

val all_paths_value: root:'root ttype -> target:'target ttype -> 'root -> ('root, 'target, Mlfi_type_path.kind) Mlfi_type_path.t list
(** Returns all the paths leading to a value of type ['target] inside
    a value of type ['root]. Only paths that are valid in the given value
    are returned. Traverses options, lists, arrays, tuples, records, and the
    active constructor of variants, but does not force lazy values or traverse
    objects, functions, or abstract values. Can loop on cyclic values. *)

val constructor_name: t:'a ttype -> 'a -> string
(** Return the name of the constructor of the argument.  Raises
    [Invalid_argument] if the type of the argument is not a sum type. *)

val smallest_size: Mlfi_types.stype -> int
(** Smallest size of a value for that type (max_int if no finite value). *)

val default_value_reaches_node: Mlfi_types.node -> Mlfi_types.stype -> bool
(** Check if the "default value" for a give stype (empty list, first constructor, etc)
    would reach the give type node *)

val enumerate: 'a ttype -> 'a list
(** A list containing all values of a sum type with constant constructors. *)

module NestedFunction: sig
  type _ t =
    | Res: 'a ttype -> 'a t (* 'a is NOT a function *)
    | Fun: string * 'b ttype * 'c t -> ('b -> 'c) t

  val get: 'a ttype -> 'a t (** Maximum decomposition of toplevel arrow types *)
end

val get_root_prop: string -> Mlfi_types.stype -> string option
(** Check if the stype has some property at the root (possibly under other properties). *)

val has_subterm: t:'t ttype -> u:'u ttype -> ('u -> bool) -> 't -> bool
(** Check if a value contains another value verifying a predicate. *)

type decision =
  | Default
  | Prune
  | Replace : 'b ttype * 'b -> decision

type override = { override: 'a. 'a ttype -> 'a -> decision }

val to_dot: ?override:override -> t:'a ttype -> Format.formatter -> 'a -> unit
(** Output a Graphviz DOT representation of a value, following the structure
    exposed by its runtime type. Shared and cyclic blocks are represented by
    shared nodes. Abstract values, functions, and objects are represented as
    leaves. *)
