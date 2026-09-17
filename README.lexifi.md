README.lexifi.md
================

LexiFi maintains a fork of the [OCaml](https://ocaml.org) system, with
extensions to the language and tools.

You compile the toolchain as usual. For example, to build the toolchain in a "local" subdirectory:
```
./configure --prefix=$(pwd)/local
make -j
make install
```
A few helper modules and an example of use is included:
- `private/mlfi_core/mlfi_types.{ml,mli}`: low-level (untyped) representation of type witnesses
- `private/mlfi_core/mlfi_xtypes.{ml,mli}`: high-level (typed) representation of type witnesses (this is the library one programs against usually)
- `private/mlfi_core/mlfi_type_path.{ml,mli}`: user library for dealing with "type paths" (type-safe descriptors of "paths" inside data structures)
- `private/mlfi_core/mlfi_json.{ml,mli}`: a json encoder/decoder using runtime types (example of use, not the most readable one because optimized for use inside LexiFi)

To see the JSON codec in action, launch the toplevel as follows:
```
$ cd private/mlfi_core
$ ../../local/bin/ocamlc -c mlfi_types.mli mlfi_types.ml mlfi_type_path.mli mlfi_type_path.ml mlfi_xtypes.mli mlfi_xtypes.ml mlfi_json.mli mlfi_json.ml
$ rlwrap ../../local/bin/ocaml mlfi_types.cmo mlfi_type_path.cmo mlfi_xtypes.cmo mlfi_json.cmo
OCaml version 5.5.1
Enter #help;; for help.

# print_endline (Mlfi_json.to_pretty_string (Mlfi_json.to_json [Ok 42; Error "bad"]));;
[
  {
    "type": "Ok",
    "val": 42
  },
  {
    "type": "Error",
    "val": "bad"
  }
]
- : unit = ()
#
```

Overview
--------
Runtime types are values of type `'a Mlfi_types.ttype` (from the custom
compiler). They let you inspect values and build typed paths.

`Mlfi_types.stype` represents an unknown type; `'a Mlfi_types.ttype` represents
the known type `'a`. Most standard types are supported (basic types, tuples,
arrows, arrays, lists, options, records, sums). Some features are not
represented (e.g. GADTs, first-class modules).

The compiler can synthesize missing labeled `~t:` arguments (the label does not
matter) when a function expects a `ttype` (see "Missing Labeled Arguments" in
AGENTS.md). You can always pass `~t` explicitly.

Key entry points:
- `[%t: T]` builds a `T ttype`.
- `Mlfi_xtypes` provides safe introspection of `ttype`s (records, sums, lists,
  props, etc.) and helpers to construct common `ttype`s.
- `Mlfi_type_path` represents typed paths inside values and types, with `[%p ...]`
  syntax for path literals.

Getting a ttype
---------------
```ocaml
let t_int = [%t: int]
let t_pair = Mlfi_xtypes.pair [%t: int] [%t: string] (* or: [%t: int * string] *)
let t_opt = Mlfi_xtypes.option [%t: float]  (* or: [%t: float option] *)

(* Convert from dynamic stype when you only have runtime stype. *)
let Mlfi_xtypes.Ttype t = Mlfi_xtypes.sttype_of_stype some_stype
```

Notes on abstract types:
- If a type is abstract in the current environment, its runtime representation
  records only the name.
- If a type abbreviation is visible, the compiler expands it.
- For abstract types, the compiler uses a
  heuristic: if a value `Foo.t : Foo.t ttype` is in scope (same path as the type), it is used as the runtime representation of `Foo.t`.

Type properties
---------------
Type properties are `(string * string)` pairs attached to types, fields, and
constructors. They flow into runtime types and can be inspected.

Syntax:
```ocaml
(* Type expression, only in type declarations *)
t [@t foo="bar"]

(* Constructor declaration and argument *)
type s = A of t [@t foo="bar"] (* property on constructor *)
type s = A of (t [@t foo="bar"]) (* property on argument *)

(* Label declaration and record field type *)
type s = { f : t [@t foo="bar"]; ... } (* property on field *)
type s = { f : (t [@t foo="bar"]); ... } (* property on the field's type *)

(* Record / variant type declaration *)
type s = { ... } [@@t foo="bar"]
type s = A of ... | ... [@@t foo="bar"]
```

Missing labeled arguments
-------------------------
When a non-optional labeled argument has type `'a Mlfi_types.ttype`, the
compiler can synthesize it. Equivalent to `[%t: _]`:
```ocaml
let f ~t x = Mlfi_types.stype_of_ttype t, x
let _ = f 42  (* implicit ~t:[%t: int] *)
```

Dynamic type equality and TypEq
-------------------------------
`Mlfi_types.ttypes_equality` compares two `ttype`s and returns a witness
`('a, 'b) Mlfi_types.TypEq.t` when they are equal. Use the GADT to recover
type equality safely.

```ocaml
let int_of (type t) (t : t ttype) (x : t) : int option =
  match Mlfi_types.ttypes_equality [%t: int] t with
  | Some Eq -> Some x
  | None -> None
```

Ignoring properties:
```ocaml
let eq =
  Mlfi_types.ttypes_equality_modulo_props
    (Mlfi_types.add_props_ttype ["foo","bar"] [%t: int])
    [%t: int]
```

Inspecting types with Mlfi_xtypes
---------------------------------
Typical flow:
1) get a `ttype`
2) check shape (`xtype_of_ttype` or `is_record`, `is_sum`, `is_list`, `is_option`, or , ...)
3) use the descriptor to inspect/build values

Polymorphic example (locally abstract type):
```ocaml
let rec show_any : type a. a ttype -> a -> string =
  fun t v ->
    match Mlfi_xtypes.xtype_of_ttype t with
    | Mlfi_xtypes.String -> v
    | Mlfi_xtypes.Int -> string_of_int v
    | Mlfi_xtypes.List (elt_t, _) ->
        "[" ^ String.concat "; " (List.map (show_any elt_t) v) ^ "]"
    | _ -> "???"
```

Records/tuples (construct + inspect):
```ocaml
match Mlfi_xtypes.is_record my_ttype with
| None -> ()
| Some record ->
    let v =
      Mlfi_xtypes.Record.build record
        { mk = fun f ->
            let _t = Mlfi_xtypes.RecordField.ttype f in
            failwith "fill"
        }
    in
    List.iter
      (fun (Mlfi_xtypes.Field f) ->
         let _name = Mlfi_xtypes.RecordField.name f in
         let _ = Mlfi_xtypes.RecordField.get f v in
         ())
      (Mlfi_xtypes.Record.fields record)
```

Sums (construct + inspect):
```ocaml
match Mlfi_xtypes.is_sum my_ttype with
| None -> ()
| Some sum ->
    let ctors = Mlfi_xtypes.Sum.constructors sum in
    let Mlfi_xtypes.Constructor c = ctors.(0) in
    let v = Mlfi_xtypes.Constructor.inject c arg_value in
    let Mlfi_xtypes.Constructor c' = Mlfi_xtypes.Sum.constructor sum v in
    let _arg = Mlfi_xtypes.Constructor.project_exn c' v in
    ()
```

Props and wrappers:
```ocaml
match Mlfi_xtypes.is_prop t with
| Some (props, sub_t) -> (* read properties *)
| None -> ()
```
Use `get_first_props_ttype` / `remove_first_props_xtype` to strip one layer.
`Mlfi_xtypes.make_abstract` wraps a `ttype` as abstract.

Typed paths with Mlfi_type_path
-------------------------------
`Mlfi_type_path` gives typed paths inside values. Paths are typed by root and
target and can be composed.

Path literals (`[%p ...]`) are parsed from ordinary expressions:
- root: `[%p]`
- field: `field` or `path.field`
- constructor: `Ctor` or `path.(Ctor)` (parentheses required in chains)
- tuple element: `n/m` or `path.(n/m)` (literal ints)
- list element: `[i]` or `path.[i]` (single-element list literal / index)
- array element: `[|i|]` or `path.(i)` (single-element array literal / index)
- optional type constraint on field/constructor: `(field : t)` or `(Ctor : t)`

Examples:
```ocaml
let p1 : (t, u, _) Mlfi_type_path.t = [%p foo.bar]
let p2 : (t, v, _) Mlfi_type_path.t = [%p foo.(Ctor).(0/2)]
let p3 : (u, w, _) Mlfi_type_path.t = [%p (attributes : basic_commodity)]
let p = Mlfi_type_path.(^^) p1 p3
```

Extract/patch:
```ocaml
let sub_t, sub = Mlfi_type_path.extract ~t:root_t p v
let v' = Mlfi_type_path.patch ~t:root_t p v (fun x -> x)
```

Notes:
- `patch` and `force_shape` do not support list/array steps.
- `has_shape` checks if a path is valid for a particular value.
- `extract_type` gives the target `ttype` without value traversal.
- `is_prefix` / `is_empty` help reason about path relationships.

Build paths from descriptors:
```ocaml
let field_path = Mlfi_xtypes.RecordField.path field
let ctor_path = Mlfi_xtypes.Constructor.path ctor
```
