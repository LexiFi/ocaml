(***************************************************************************)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


(** {2 Representation of JSON trees} *)

type error = string

module Number: sig
  type t
  val to_float: t -> float

  val to_int: t -> int option
  (** Return an integer if the number can be faithfully represented as one *)

  val round_to_int: t -> int
  (** Same as [int_of_float (to_float ...)] *)

  val to_string: t -> string
  (** Print number using JSON syntax (integers printed without a decimal separator) *)

  val of_int: int -> t
  val of_float: float -> t
end

type value = private
  | Null
  | Bool of bool
  | Number of Number.t
  | String of string
  | Array of value list
  | Object of (string * value) list

val null: value
val bool: bool -> value
val int: int -> value
val float: float -> value
val string: string -> value
val array: value list -> value
val object_: (string * value) list -> value

val number: Number.t -> value

(** Notes:
    - The [String] payload is either Latin1-encoded or utf8-encoded,
      depending on the [utf8] flag passed to [encode/decode].
      When not in utf8 mode, a code point outside the range of that character set will
      be encoded in a special (undocumented) way.
*)


(** {2 Mapping between JSON trees and their textual representation} *)

val encode: value -> string
(** Encode JSON tree into a compact JSON text (single line,  etc). *)

val decode: ?filename:string -> string -> (value, error) result
(** Parse a JSON text into JSON tree.

    The optional [filename] argument is used to report locations in error messages.
*)

val decode_many: ?filename:string -> string -> (value list, error) result
(** Like [decode], but decode many JSON values, concatenated one after the
    other. *)

type pretty_options =
  {
    compact: bool option;
  }
(* This type is exposed to the template language *)

val to_pretty_string: ?options:pretty_options -> value -> string
(** Prints a JSON tree into human-friendly text (multi-line, indentation). *)

val to_indent_based_string: value -> string
(** Prints a JSON tree into a stripped down human-friendly text (no braces, quotation marks not semicolons).

    Example excerpt:

    redemption_date: 2022-10-28
    underlyings:
      0:
        bloomberg_ticker: BNP FP Equity
*)

type ctx
(** Context storing serialization variations directives.

    Variations:
    - to_json_field: how to translate a json field name to an mlfi record field.
    - to_json: local custom JSON conversion for selected types.
    - of_json: local custom JSON parsing for selected types.
*)

type to_json_override =
  {
    to_json: 'a. 'a ttype -> ('a -> value) option;
  }
(** Local conversion override used by JSON serialization. Returning
    [Some to_json] serializes values of the current type with [to_json];
    returning [None] falls back to the default structural conversion. The
    override is called once while the conversion function for the type is
    staged, not once per serialized value. *)

type of_json_override =
  {
    of_json: 'a. 'a ttype -> (value -> ('a, error) result) option;
  }
(** Local conversion override used by JSON deserialization. Returning
    [Some of_json] parses values of the current type with [of_json];
    returning [None] falls back to the default structural conversion. *)

val ctx:
  ?to_json_field:(string -> string) ->
  ?lossy:unit ->
  ?to_json:to_json_override ->
  ?of_json:of_json_override ->
  unit ->
  ctx

val allow_verbatim_ctx: unit -> ctx
(** ctx where an ocaml field that starts with __ is used verbatim as json field
    (e.g. __type -> type).
*)

val caml_case_ctx: ?allow_verbatim:unit -> unit -> ctx
(** ctx where to_json_field transforms ocaml fields to caml case json fields (e.g. foo_bar_baz -> fooBarBaz).
    If the allow_verbatim flag is set, the rest of an ocaml field that starts
    with __ is used verbatim as json field (e.g. __type -> type).
*)

val pascal_case_ctx: ?allow_verbatim:unit -> unit -> ctx
(** ctx where to_json_field transforms ocaml fields to pascal case json fields (e.g. foo_bar_baz -> FooBarBaz).
    If the allow_verbatim flag is set, the rest of an ocaml field that starts
    with __ is used verbatim as json field (e.g. __type -> type).
*)

val trim_fields_ctx: unit -> ctx
(** ctx where to_json_field simply trims leading and trailing underscores from ocaml field to
    be used as json fields (e.g. __foo_bar_baz___ -> foo_bar_baz).
*)

val deserialize: ?filename:string -> ?ctx:ctx -> t:'a ttype -> string -> ('a, error) result
(** Parse a JSON text and deserialize it directly to an OCaml value. This is
    equivalent to [Result.bind (decode ?filename s) (of_json ?ctx ~t)]. *)

(** {2 Typeful generic mapping between JSON trees and OCaml values} *)

type json_signal_handlers = {
  object_open: (unit -> unit);
  object_field: (string -> unit);
  object_close: (unit -> unit);
  array_open: (unit -> unit);
  array_close: (unit -> unit);
  comma: (unit -> unit);
  int: (int -> unit);
  float: (float -> unit);
  bool: (bool -> unit);
  string: (string -> unit);
  null: (unit -> unit);
  value: (value -> unit);
}

val to_json_stream: ctx -> 'a Mlfi_xtypes.xtype -> (json_signal_handlers -> 'a -> unit)
(** [to_json_stream ctx t handlers x] maps an OCaml value to a stream of JSON
    signals by calling the appropriate handlers. This is more efficient than
    [to_json] for large values as it avoids building the intermediate JSON tree.

    Partial application on [~ctx] and [~t] is possible, and produce an
    optimized function. Raises [Invalid_argument] on encoding failure.
*)

val value_to_signal_stream: json_signal_handlers -> value -> unit
(** [value_to_signal_stream handlers v] converts a JSON value to a stream of
    signals by calling the appropriate handlers. This is used internally by
    streaming functions when they encounter existing JSON values. *)

val to_json_string_stream: ?ctx:ctx -> t:'a ttype -> (string -> unit) -> 'a -> unit
(** [to_json_string_stream ~t output x] converts an OCaml value directly to
    a stream of JSON string fragments by calling [output] with each fragment.
    Raises [Invalid_argument] on encoding failure. *)

val to_json: ?ctx:ctx -> t:'a ttype -> 'a -> value
(**
   [to_json x] maps an OCaml value to a JSON tree representing the same
   information.  The mapping is driven by the type of [x] and the
   default behavior (which can be overridden) is defined below.

   Basic types:
   - 1                 ---> 1
   - 1.                ---> 1.
   - ()                ---> \{\}
   - true/false        ---> true/false
   - "abc"             ---> "abc"
   - 2001-01-01        ---> "2001-01-01"

   List/array/tuple types:
   - [x; y]            ---> [x', y']
   - [|x; y|]          ---> [x', y']
   - (x, y)            ---> [x', y']

   Record types:
   - \{l1 = x; l2 = y\}    ---> \{"l1": x', "l2": y'\}
   - \{l1 = x; l2 = None\} ---> \{"l1": x'\}

   Sum types:
   - A                    ---> \{"type": "A"\}
   - B x                  ---> \{"type": "B", "val": [x']\}
   - C (x, y)             ---> \{"type": "C", "val": [x', y']\}
   - D \{l1 = x; l2 = y\} ---> \{"type": "D", "l1": x', "l2": y'\}

   Option types:
   - Some x            ---> x'
   - None              ---> null (when not in record)

   Nested option types!
   - Some (Some x)    ---> \{"type": "Some", "val": x'\}
   - Some None        ---> \{"type": "Some"\}
   - None             ---> \{"type": "None"\}

   Lazy types:
   - lazy x            ---> x'
     (x is forced upon jsonification; de-jsonification is lazy)

   String Maps ('a Mlfi_sets_maps.StringMap.t)
     are mapped to objects

     e.g.
     \{"a" -> 1; "b" -> 2\}                 ---> \{"a": 1, "b": 2\}
     \{"foo" -> "hello"; "bar" -> "world"\} ---> \{"foo": "hello", "bar": "world"\}

   Special cases:
   - (x : Mlfi_isdatypes.variant) ---->  "<textual representation of x, in OCaml syntax>"
   - (x : Mlfi_json.value)        ---->  x


   Notes:

   - Function types and object types are not supported.

   - Upon parsing, extra fields in objects are accepted and ignored
     (including when parsing a sum type or unit).

   - Special float values (nan, infinity) are not supported (but
     not explicitly checked).

   - A constructor or a sum type can be annotated with [@t as_json_string].
     This will have the effect of mapping that constructor (or all constant
     constructors, if applied to the whole type) to/from plain strings.

   - TODO: support some type properties (default value, etc).

   Raises [Invalid_argument] on encoding failure.
*)

val to_json_string: ?ctx:ctx -> t:'a ttype -> ('a -> string)
(** [to_json_string ~t x] converts an OCaml value to a JSON string
    representation. This is equivalent to
    [encode (to_json ~t x)].

    Partial application on [~ctx] and [~t] is possible. Raises
    [Invalid_argument] on encoding failure.
*)

val to_json_buffer: ctx -> 'a Mlfi_xtypes.xtype -> (Buffer.t -> 'a -> unit)
(** [to_json_buffer ctx t buf x] converts an OCaml value to a JSON string
    representation and appends it to the give buffer. This is equivalent to
    [Buffer.add_string buf (to_json_string ~t:(ttype_of_xtype t) x)].

    Partial application on [~ctx] and [~t] is possible. Raises
    [Invalid_argument] on encoding failure.
*)

val of_json: ?ctx:ctx -> t:'a ttype -> value -> ('a, error) result
(** Reverse mapping.  If [to_json ~t x] succeeds, the property
    [of_json ~t (to_json ~t x) = Ok x] is expected to hold
    (except corner cases such as unchecked special float values,
    and assuming that custom converters behaves properly).
*)

val variant_to_json_lossy: Mlfi_isdatypes.variant -> value
(** A "lossy" conversion that maps variants to JSON values in a more idiomatic
    way (None => Null, etc). *)

val to_variant: value -> Mlfi_isdatypes.variant
(** Satisfies: [variant_to_json_lossy (to_variant v) = v] *)

(** {2 Custom mapping for specific types} *)

val register_conversion:
  t:'a ttype ->
  to_json:('a -> value) ->
  of_json:(value -> ('a, error) result) ->
  unit

(** [register_conversion] registers a global custom mapping between
    OCaml values and JSON trees for a specific closed *abstract* or *sum/record* type.

    It is not allowed to use [null] in the JSON representation of
    values, at least if the type is used under the option type
    constructor ([null] is reserved for reprensenting the
    [None] case).

    [to_json] and [of_json] should not raise, except in case of programmer error.
*)

module type ABSTRACT_1_CONVERSION =
sig
  type 'a t
  val t: unit t ttype
  val to_json: t:'a ttype -> ?ctx:ctx -> 'a t -> value
  val of_json: t:'a ttype -> ?ctx:ctx -> value -> ('a t, error) result
end

val register_parametric_conversion: (module ABSTRACT_1_CONVERSION) -> unit
(** [register_parametric_conversion] registers a global custom
    mapping for a parametric abstract type.
*)

module OpenAPI : sig
  type components

  val schema_of_type: components -> 'a ttype -> value
  (** Returns an OpenAPI description of the argument, according to the encoding
      used by the [of_json] and [to_json] functions. *)

  val empty_components: unit -> components
  val component_schemas: components -> (string * value) list
end

val of_get_params: (string * string) list -> value
val to_get_params: value -> (string * string) list

module Access: sig

  type step =
    | Nth of int
    | Key of string

  (** A description of paths inside a JSON value. *)
  type path = step list

  (** A description of JSON types. *)
  type typ =
    | TyArray
    | TyObject
    | TyString
    | TyBool
    | TyNumber
    | TyDate
    | TyNull

  type error_kind =
    | Key_unbound of string
    | Msg of string
    | Nth_unbound of int
    | Type_error of typ * typ
    | Alt_error of error * error

  (** The type of errors. [path] is the path inside the JSON value pointing to
      the element that caused the error (in reverse order). [error_kind]
      describes the kind of error. *)
  and error = path * error_kind

  val string_of_error: error -> string

  type 'a t
  (** The type of JSON "structural" parsers. A parser either succeeds against a
      JSON value and produces a value of type ['a], or it fails. *)

  val app: ('a -> 'b) t -> 'a t -> 'b t
  (** [app fq q] parses a JSON value with [fq] and [q] and applies the result
      of the latter to the former. *)

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  (** [bind q f] parses a JSON value with [q], if successful, the same value is
      parsed with [f] applied to the result of the first parser. *)

  val const: 'a -> 'a t
  (** [const v] is a parser that always succeeds with value [v]. *)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map f q] is a parser that applies [q] and returns [f] applied to its
      result in case of success. *)

  val fail: string -> 'a t
  (** [fail s] is a parser that always fails with message [s]. *)

  val failf: ('a, unit, string, 'b t) format4 -> 'a
  (** [failf fmt] is a parser that always fails with message [fmt] (a format
      string). *)

  val try_: 'a t -> ('a, error) Result.t t
  (** [try_ p] is [Ok x] if [p] succeeds with result [x] or [Error err] if it
      fails with error [err]. *)

  val alt: 'a t -> 'a t -> 'a t
  (** [alt p q] applies [p] and, if it fails, applies [q]. *)

  val pair: 'a t -> 'b t -> ('a * 'b) t
  (** [pair q r] is a parser that applies [q] and [r] to the JSON value and
      returns the pair of results (if successful). *)

  val query: 'a t -> value -> ('a, error) Result.t
  (** [query q v] executes the parser [q] on a JSON value [v]. *)

  val value: value t
  (** [value] is a parser that expects an arbitrary JSON value, which it
      returns. *)

  val string: string t
  (** [string] is a parser that expects a JSON string, which it returns. *)

  val int: int t
  (** [int] is a parser that expects an integer (this is a LexiFi extensino to
      JSON format), which it returns. *)

  val float: float t
  (** [float] is a parser that expects a JSON number, which it returns. *)

  val percentage: float t
  (** Same as [float] but divides the result by 100. *)

  val or_null: 'a t -> 'a option t
  (** [or_null q] is [None] on [null] and [q] otherwise. *)

  val or_null_empty: 'a t -> 'a option t
  (** [or_null_empty q] is [None] on [null] and empty string, and [q] otherwise. *)

  val date: Mlfi_date.t t

  val bool: bool t
  (** [bool] is a parser that expects a JSON boolean, which it returns. *)

  val list: 'a t -> 'a list t
  (** [list q] is a parser that expects a JSON array, in which case it applies
      [q] to each one of its elements. *)

  val builder: ('a -> unit) t list -> (('a -> unit) -> 'b) -> 'b t

  val fold_assoc: (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b t
  (** [fold_assoc f q x] expects a JSON object, and folds [f] over its fields
      (left-to-right), applying [q] to each value. *)

  val assoc: 'a t -> (string * 'a) list t
  (** [assoc q] expects a JSON object, and returns the list of [(key, value)]
      pairs, after applying [q] to each value. *)

  val hd: 'a t -> 'a t
  (** [hd q] expects a JSON array, and applies [q] to its first element. *)

  val tl: 'a t -> 'a list t
  (** [tl q] expects a JSON array, and applies [q] to all its elements,
      except the first one. *)

  val last: 'a t -> 'a t
  (** [last q] expects a JSON array, and applies [q] to its last elements. *)

  val member: ?equal:(string -> string -> bool) -> ?default:'a -> string -> 'a t -> 'a t
  (** [member ?default s q] extracts field [s] of the JSON value and applies [q]
      to it. If the field does not exist, the parser succeeds with [default] (if
      given), otherwise it fails. *)

  val member_opt: string -> 'a t -> 'a option t
  (** [member_opt s q] extracts field [s] of the JSON value and applies [q] to it.
      If the field does not exist or it has value [null] then it succeeds with
      [None]. *)

  val member_try: string -> 'a t -> 'a option t
  (** Same as [member_opt], but ignores all parsing error under the field (return None). *)

  val empty_none: 'a t -> 'a t
  (** Same as the argument, but fail the empty string. *)

  module Infix : sig
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
    (** Same as {!bind} *)

    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    (** Same as {!app} (with arguments flipped). *)

    val (and+) : 'a t -> 'b t -> ('a * 'b) t
    (** Same as {!pair}. *)

    val (and*) : 'a t -> 'b t -> ('a * 'b) t
    (** Same as {!pair}. *)

    val ($) : ('a -> 'b) -> 'a t -> 'b t
    (** Same as {!app}. *)
  end
end

module Weighted: sig
  type t
  (** A weighted data structure used to visualize the differences between a list of JSON objects with similar format *)

  val of_json_list : value list -> t
  val to_string : t -> string
end


module Annotated : sig
  type 'a desc =
    | Atom of value
    | Array of 'a list
    | Object of (string * 'a) list
end

val decode_with_loc: ?filename:string -> (start:int -> stop:int -> 'a Annotated.desc -> 'a) -> string -> ('a, error) result
(** start: index of the first byte for the sub-value.
    stop: index of the first byte after the sub-value. *)
