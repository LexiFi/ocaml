(***************************************************************************)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


let cast_ttype: Mlfi_types.stype -> 'a ttype = Obj.magic


let option (type t) (t : t ttype) = [%t: t option]
let list (type t) (t : t ttype) = [%t: t list]
let array (type t) (t : t ttype) = [%t: t array]
let lazyt (type t) (t : t ttype) = [%t: t Lazy.t]

let pair (type t1 t2) (t1 : t1 ttype) (t2 : t2 ttype) = [%t: t1 * t2]

let triple (type t1 t2 t3) (t1 : t1 ttype) (t2 : t2 ttype) (t3 : t3 ttype) =
  [%t: t1 * t2 * t3]

let quartet (type t1 t2 t3 t4)
    (t1 : t1 ttype) (t2 : t2 ttype) (t3 : t3 ttype) (t4 : t4 ttype) =
  [%t: t1 * t2 * t3 * t4]

let quintet (type t1 t2 t3 t4 t5)
    (t1 : t1 ttype) (t2 : t2 ttype) (t3 : t3 ttype) (t4 : t4 ttype) (t5 : t5 ttype) =
  [%t: t1 * t2 * t3 * t4 * t5]

let arrow ?(label = "") t1 t2 = cast_ttype (DT_arrow (label, Mlfi_types.stype_of_ttype t1, Mlfi_types.stype_of_ttype t2))

let result (type t1 t2) (t1 : t1 ttype) (t2 : t2 ttype) =
  [%t: (t1, t2) result]

let unlistt t =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_list t -> Obj.magic t
  | _ -> assert false

let unlazyt t =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_abstract ("lazy_t", [t]) -> Obj.magic t
  | _ -> assert false


type 'a record_builder = Obj.t option array

let path_of_steps: Mlfi_type_path.Internal.step list -> ('a, 'b, 'c) Mlfi_type_path.t = Obj.magic

let dummy_xtype = Obj.repr "foo"

module RecordField = struct
  type ('s, 't) t =
    {
      rank: int;
      t: 't ttype;
      name: string;
      props: (string * string) list;
      is_float_record: bool;

      mutable xtype: Obj.t;
    }

  let ttype r = r.t
  let name r = r.name
  let props r = r.props
  let get r x =
    let x = Obj.repr x in
    if r.is_float_record then Obj.magic (Obj.double_field x r.rank)
    else Obj.magic (Obj.field x r.rank)
  let set r b x = Array.unsafe_set b r.rank (Some (Obj.repr x))
  let field_path = function
    | {name = ""; _} -> invalid_arg "RecordField.field_path: not a record"
    | {name; _} -> path_of_steps [Mlfi_type_path.Internal.Field name]
  let path = function
    | {name = ""; rank; _} -> path_of_steps [Mlfi_type_path.Internal.Tuple_nth rank]
    | {name; _} -> path_of_steps [Mlfi_type_path.Internal.Field name]
end

type 's has_record_field = Field: ('s, 't) RecordField.t -> 's has_record_field

type 's field_builder = { mk: 't. ('s, 't) RecordField.t -> 't }
[@@ocaml.unboxed]

module Record = struct
  type 's memo = ..
  type 's t =
    {
      ttype: 's ttype;
      fields: 's has_record_field list;
      make: default:'s option -> ('s record_builder -> unit) -> 's;
      build: 's field_builder -> 's;
      find_field: (string -> 's has_record_field option);
      find_field_typed: 't. (('s, 't) Mlfi_type_path.field -> ('s, 't) RecordField.t);
      mutable memo: 's memo array;
    }

  let ttype r = r.ttype
  let fields r = r.fields
  let make ?default r x = r.make ~default x

  let build r x = r.build x
  let find_field r s = r.find_field s
  let find_field_typed r s = r.find_field_typed s


  let memo r = r.memo
  let set_memo r x = r.memo <- x

  let path x =
    match Mlfi_types.stype_of_ttype x.ttype with
    | DT_node{rec_name=s; _} -> s
    | _ -> assert false
end

module Constructor = struct
  type ('s, 't) memo = ..
  type ('s, 't) t =
    {
      index: int;
      t: 't ttype;
      name: string;
      props: (string * string) list;
      project: 's -> 't;
      inject: 't -> 's;
      path: ('s, 't) Mlfi_type_path.constructor;
      nb_args: int;
      mutable xtype: Obj.t;
      mutable memo: ('s, 't) memo array;
    }

  let ttype r = r.t
  let index r = r.index
  let name r = r.name
  let props r = r.props
  let project_exn r x = r.project x
  let project r x = try Some (r.project x) with Not_found -> None
  let inject r x = r.inject x
  let path r = r.path
  let nb_args r = r.nb_args
  let memo r = r.memo
  let set_memo r x = r.memo <- x
end

type 's has_constructor = Constructor: ('s, 't) Constructor.t -> 's has_constructor

module Sum = struct
  type 's memo = ..
  type 's t =
    {
      ttype: 's ttype;
      constructors: 's has_constructor array;
      get_constructor_index: 's -> int;
      lookup_constructor: string -> int;
      mutable memo: 's memo array;
    }

  let ttype x = x.ttype
  let path x =
    match Mlfi_types.stype_of_ttype x.ttype with
    | DT_node{rec_name=s; _} -> s
    | _ -> assert false

  let constructors x = x.constructors
  let get_constructor_index x y = x.get_constructor_index y
  let lookup_constructor x y = x.lookup_constructor y
  let constructor x y = x.constructors.(x.get_constructor_index y)

  let is_enum sum =
    let is_const (Constructor c) =
      match Mlfi_types.ttypes_equality_modulo_props (Constructor.ttype c) [%t: unit] with
      | Some Eq -> true
      | None -> false
    in
    Array.for_all is_const (constructors sum)

  let memo r = r.memo
  let set_memo r x = r.memo <- x
end

module Method = struct
  type ('s, 't) t =
    {
      t: 't ttype;
      name: string;
      call: 's -> 't;

      mutable xtype: Obj.t;
    }

  let ttype m = m.t
  let name m = m.name
  let call m x = m.call x
end

type 's has_method = Method: ('s, 't) Method.t -> 's has_method

module Object = struct
  type 's t =
    {
      ttype: 's ttype;
      methods: 's has_method list;
    }

  let ttype o = o.ttype
  let methods o = o.methods
end

type 'a is_function = Function: (string * 'b ttype * 'c ttype) -> ('b -> 'c) is_function
type 's is_list = List: 't ttype -> ('t list) is_list
type 's is_lazy = Lazy: 't ttype -> ('t Lazy.t) is_lazy
type 's is_array = Array: 't ttype -> ('t array) is_array
type 's is_option = Option: 't ttype -> ('t option) is_option
type 's is_tuple2 = Tuple2: ('a ttype * 'b ttype) -> ('a * 'b) is_tuple2
type 's is_tuple3 = Tuple3: ('a ttype * 'b ttype * 'c ttype) -> ('a * 'b * 'c) is_tuple3
type _ is_result = Result: ('a ttype * 'b ttype) -> ('a, 'b) Stdlib.result is_result

let is_list (type s_ t_) (s_ttype: s_ ttype): s_ is_list option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_list t_stype -> Some (Obj.magic (List (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let is_lazy (type s_ t_) (s_ttype: s_ ttype): s_ is_lazy option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_abstract ("lazy_t", [t_stype]) -> Some (Obj.magic (Lazy (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let is_result (type s_ t_) (s_ttype: s_ ttype): s_ is_result option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_node {rec_name = "Stdlib.result"; rec_args=[t1_stype; t2_stype]; _} ->
      Some (Obj.magic
              (Result ((cast_ttype t1_stype: t_ ttype),
                       (cast_ttype t2_stype: t_ ttype))))
  | _ -> None

let is_array (type s_ t_) (s_ttype: s_ ttype): s_ is_array option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_array t_stype -> Some (Obj.magic (Array (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let is_tuple2 (type s_ t1_ t2_) (s_ttype: s_ ttype) : s_ is_tuple2 option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_tuple [t1_stype; t2_stype] ->
      Some (Obj.magic
              (Tuple2 ((cast_ttype t1_stype: t1_ ttype),
                       (cast_ttype t2_stype: t2_ ttype))))
  | _ -> None

let is_tuple3 (type s_ t1_ t2_ t3_) (s_ttype: s_ ttype) : s_ is_tuple3 option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_tuple [t1_stype; t2_stype; t3_stype] ->
      Some (Obj.magic
              (Tuple3 ((cast_ttype t1_stype: t1_ ttype),
                       (cast_ttype t2_stype: t2_ ttype),
                       (cast_ttype t3_stype: t3_ ttype))))
  | _ -> None

let is_option (type s_ t_) (s_ttype: s_ ttype): s_ is_option option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_option t_stype -> Some (Obj.magic (Option (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let build_object s_ttype methods : _ Object.t =
  let prepare (type t_) (name, t_stype) =
    let t = (cast_ttype t_stype: t_ ttype) in
    let label = CamlinternalOO.public_method_label name in
    let call (x: t_) = Obj.magic (CamlinternalOO.send (Obj.magic x) label) in
    Method {t; name; call; xtype = dummy_xtype}
  in
  {
    ttype = s_ttype;
    methods = List.map prepare methods;
  }

let is_object (type s_) (s_ttype: s_ ttype) : s_ Object.t option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_object methods -> Some (build_object s_ttype methods)
  | _ -> None

exception Missing_field_in_record_builder

let build_record (type s_) ttype record_repr record_fields : s_ Record.t =
  let len = List.length record_fields in
  let fields =
    List.mapi
      (fun (type t_) i (name, props, t_stype) ->
         let t = (cast_ttype t_stype: t_ ttype) in
         Field
           {
             rank = i;
             t;
             name;
             props;
             xtype = dummy_xtype;
             is_float_record = match record_repr with Mlfi_types.Record_float -> true | _ -> false;
           }
      )
      record_fields
  in
  let make ~default f =
    let b = Array.make len None in
    f b;
    match record_repr with
    | Record_regular | Record_inline _ ->
        (* we could copy b directly except if it is a float array *)
        let r =
          match default with
          | None ->
              let tag = match record_repr with Mlfi_types.Record_inline tag -> tag | _ -> 0 in
              Obj.new_block tag len
          | Some default -> Obj.dup (Obj.magic default)
        in
        for i = 0 to len - 1 do
          match Array.unsafe_get b i with
          | None -> if default = None then raise Missing_field_in_record_builder
          | Some x -> Obj.set_field r i x
        done;
        Obj.magic r
    | Record_float ->
        let b = Float.Array.init (Array.length b)
            (fun i -> match b.(i) with
               | Some x -> Obj.obj x
               | None ->
                   match default with
                   | Some default -> Float.Array.get (Obj.magic default: floatarray) i
                   | None -> raise Missing_field_in_record_builder
            )
        in
        Obj.magic b
    | Record_unboxed ->
        begin match b.(0), default with
        | Some x, _ -> Obj.obj x
        | None, Some default -> default
        | None, None -> raise Missing_field_in_record_builder
        end
  in
  let fields_arr = Array.of_list fields in
  let build f =
    match record_repr with
    | Record_regular | Record_inline _ ->
        let tag = match record_repr with Mlfi_types.Record_inline tag -> tag | _ -> 0 in
        let r = Obj.new_block tag len in
        for i = 0 to len - 1 do
          let (Field field) = Array.unsafe_get fields_arr i in
          Obj.set_field r i (Obj.repr (f.mk field))
        done;
        Obj.magic r
    | Record_float ->
        let r = Float.Array.create len in
        for i = 0 to len - 1 do
          let (Field field) = Array.unsafe_get fields_arr i in
          Float.Array.unsafe_set r i (Obj.magic (f.mk field) : float)
        done;
        Obj.magic r
    | Record_unboxed ->
        let (Field field) = Array.unsafe_get fields_arr 0 in
        Obj.magic (f.mk field)
  in
  let tbl = lazy (Mlfi_string.Tbl.prepare (List.map (fun (Field f) -> f.name) fields)) in
  let find_field s =
    let idx = Mlfi_string.Tbl.lookup (Lazy.force tbl) s in
    if idx < 0 then None else Some (fields_arr.(idx))
  in
  let find_field_typed f =
    match find_field (Mlfi_type_path.field_name f) with
    | Some (Field field) -> Obj.magic field
    | _ -> assert false
  in
  {ttype; fields; make; build; find_field; find_field_typed; memo = [||]}

let make len = fun ?default f ->
  let b = Array.make len None in
  f b;
  let r =
    match default with
    | None -> Obj.new_block 0 len
    | Some default -> Obj.dup (Obj.magic default)
  in
  for i = 0 to len - 1 do
    match Array.unsafe_get b i with
    | None -> if default = None then raise Missing_field_in_record_builder
    | Some x -> Obj.set_field r i x
  done;
  Obj.magic r

let makes = Array.init 500 (fun i -> Obj.repr (make i))

let build_tuple (type s_) ttype record_fields : s_ Record.t =
  (* TODO: build the fields_arr directly *)
  let fields =
    List.mapi
      (fun (type t_) i t_stype ->
         let t = (cast_ttype t_stype: t_ ttype) in
         Field {RecordField.rank = i; t; name = ""; props = [];
                xtype = dummy_xtype; is_float_record = false;
               }
      )
      record_fields
  in
  let make = Obj.obj (makes.(List.length record_fields)) in
  let fields_arr = Array.of_list fields in
  let build f =
    let len = Array.length fields_arr in
    let r = Obj.new_block 0 len in
    for i = 0 to len - 1 do
      let (Field field) = Array.unsafe_get fields_arr i in
      Obj.set_field r i (Obj.repr (f.mk field))
    done;
    Obj.magic r
  in
  let find_field_typed _ = assert false in
  {ttype; fields; make; build; find_field = (fun _ -> None); find_field_typed; memo = [||]}

let is_tuple s_ttype  : _ Record.t option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_tuple tl ->
      Some (build_tuple s_ttype tl)
  | _ ->
      None

let t_unit = Mlfi_types.stype_of_ttype [%t: unit]

let build_sum ttype variant_repr variant_constrs : _ Sum.t =
  let cst_ids = ref [] in
  let noncst_ids = ref [] in
  let constructors =
    let nb_cst = ref 0 in
    let nb_noncst = ref 0 in
    List.mapi
      (fun (type t_) i (name, props, tl) ->
         let mk nb_args stype project inject =
           Constructor
             {
               index = i;
               t = (cast_ttype stype: t_ ttype);
               name;
               props;
               project = Obj.magic project;
               inject = Obj.magic inject;
               path = path_of_steps [Mlfi_type_path.Internal.Constructor (name, nb_args)];
               nb_args;
               xtype = dummy_xtype;
               memo = [||];
             }
         in
         let mk_noncst nb_args stype project inject =
           let tag = !nb_noncst in
           noncst_ids := i :: !noncst_ids;
           incr nb_noncst;
           mk nb_args stype
             (fun x -> if Obj.tag x = tag then project x else raise Not_found)
             (inject tag)
         in
         match variant_repr with
         | Mlfi_types.Variant_unboxed ->
             let t = match tl with Mlfi_types.C_tuple [t] | C_inline t -> t | _ -> assert false in
             mk 1 t Fun.id Fun.id
         | Variant_regular ->
             match tl with
             | Mlfi_types.C_tuple [] ->
                 let tag = Obj.magic !nb_cst in
                 cst_ids := i :: !cst_ids;
                 incr nb_cst;
                 mk 0 t_unit
                   (fun x ->
                      if x != tag then (* if x is a block, it won't be equal to the tag *)
                        raise Not_found)
                   (fun () -> tag)
             | C_tuple [t] ->
                 mk_noncst 1 t
                   (fun x -> Obj.field x 0 (* if x is a constant constructor, Obj.tag x = 1000 *))
                   (fun tag x -> let r = Obj.new_block tag 1 in Obj.set_field r 0 x; r)
             | C_tuple tl ->
                 mk_noncst (List.length tl) (DT_tuple tl)
                   (fun x -> Obj.with_tag 0 x)
                   (fun tag x -> Obj.with_tag tag x)
             | C_inline t ->
                 mk_noncst 1 t
                   (fun x -> x)
                   (fun tag x -> assert (Obj.tag (Obj.repr x) = tag); x)
      )
      variant_constrs
  in
  let cst_ids = Mlfi_array.of_list_rev !cst_ids in
  let noncst_ids = Mlfi_array.of_list_rev !noncst_ids in
  let get_constructor_index =
    match variant_repr with
    | Variant_unboxed ->
        fun _ -> 0
    | Variant_regular ->
        fun x ->
          let x = Obj.repr x in
          if Obj.is_int x then Array.unsafe_get cst_ids (Obj.magic x)
          else Array.unsafe_get noncst_ids (Obj.tag x)
  in
  let tbl = lazy (Mlfi_string.Tbl.prepare (List.map (fun (name, _, _) -> name) variant_constrs)) in
  let lookup_constructor s = Mlfi_string.Tbl.lookup (Lazy.force tbl) s in
  {
    ttype;
    constructors = Array.of_list constructors;
    get_constructor_index;
    lookup_constructor;
    memo = [||];
  }

let is_function (t : 'a ttype) (type t1_ t2_) : 'a is_function option =
  match Mlfi_types.stype_of_ttype t with
  | DT_arrow (s, t1, t2) ->
      let t1_ttype = (cast_ttype t1: t1_ ttype) in
      let t2_ttype = (cast_ttype t2: t2_ ttype) in
      Some (Obj.magic (Function (s, t1_ttype, t2_ttype)))
  | _ -> None

let is_prop (type s_) (s_ttype: s_ ttype) =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_prop (prop, t) -> Some (prop, (cast_ttype t: s_ ttype))
  | _ -> None

let is_abstract (type s_) (s_ttype: s_ ttype) =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_abstract ("lazy_t", [_])
  | DT_abstract (("char" | "int32" | "int64" | "nativeint"), []) -> None
  | DT_abstract (name, args) -> Some (name, s_ttype, args)
  | _ -> None

module type ABSTRACT_1 =
sig
  type 'a t
  val t: unit t ttype
end

let abstract_1_name t =
  match Mlfi_types.stype_of_ttype t with
  | DT_abstract (s, [DT_node{rec_name="unit"; _}]) -> s
  | x -> failwith (Format.asprintf "Mlfi_xtypes: invalid ABSTRACT_1 witness: %a" Mlfi_types.print_stype x)

module type ABSTRACT_1_MATCHER_SIG = sig
  type 'a t
  type _ is_t = Is: 'b ttype * ('a, 'b t) Mlfi_types.TypEq.t -> 'a is_t
  val is_t: 'a ttype -> 'a is_t option
end

module ABSTRACT_1_MATCHER (T : ABSTRACT_1) = struct
  let name = abstract_1_name T.t

  type 'a t = 'a T.t

  type _ is_t = Is: 'b ttype * ('a, 'b T.t) Mlfi_types.TypEq.t -> 'a is_t

  let is_t (type s) (t : s ttype) : s is_t option =
    match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
    | DT_abstract (s, [t1]) when s = name ->
        Some (Is (cast_ttype t1, Obj.magic (Mlfi_types.TypEq.refl)))
    | _ ->
        None
end

module COMPOSE_ABSTRACT_1_MATCHER (T : ABSTRACT_1_MATCHER_SIG) (S : ABSTRACT_1_MATCHER_SIG) =
struct
  type 'a t = 'a T.t S.t

  type _ is_t = Is: 'b ttype * ('a, 'b T.t S.t) Mlfi_types.TypEq.t -> 'a is_t

  let is_t t =
    match S.is_t t with
    | Some (S.Is (t, eq2)) ->
        begin match T.is_t t with
        | Some (T.Is (t, eq1)) ->
            let module Lift = Mlfi_types.TypEq.Lift (struct type 'a c = 'a S.t end) in
            Some (Is (t, Mlfi_types.TypEq.trans eq2 (Lift.eq eq1)))
        | None ->
            None
        end
    | None ->
        None
end

let make_abstract t =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_node {rec_name=name; rec_args=l; _} -> Obj.magic (Mlfi_types.DT_abstract(name, l))
  | _ -> assert false

type 'a xtype
  = Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  | Date: Mlfi_date.t xtype
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | Option: 'b ttype * 'b xtype Lazy.t -> 'b option xtype
  | List: 'b ttype * 'b xtype Lazy.t -> 'b list xtype
  | Array: 'b ttype * 'b xtype Lazy.t -> 'b array xtype
  | Floatarray : floatarray xtype
  | Function: (string * ('b ttype * 'b xtype Lazy.t) * ('c ttype * 'c xtype Lazy.t)) -> ('b -> 'c) xtype
  | Sum: 'a Sum.t -> 'a xtype
  | Tuple: 'a Record.t -> 'a xtype
  | Record: 'a Record.t -> 'a xtype
  | Lazy: ('b ttype * 'b xtype Lazy.t) -> 'b Lazy.t xtype
  | Prop: ((string * string) list * 'a ttype * 'a xtype Lazy.t) -> 'a xtype
  | Object: 'a Object.t -> 'a xtype
  | Abstract: (string * 'a ttype * Mlfi_types.stype list) -> 'a xtype

let ttype_of_xtype : type t. t xtype -> t ttype = function
  | Unit -> [%t: unit]
  | Bool -> [%t: bool]
  | Int -> [%t: int]
  | Float -> [%t: float]
  | String -> [%t: string]
  | Date -> [%t: Mlfi_date.t]
  | Char -> [%t: char]
  | Int32 -> [%t: int32]
  | Int64 -> [%t: int64]
  | Nativeint -> [%t: nativeint]
  | Option (t, _) -> option t
  | List (t, _) -> list t
  | Array (t, _) -> array t
  | Floatarray -> [%t: floatarray]
  | Function (label, (t1, _), (t2, _)) -> arrow ~label t1 t2
  | Sum sum -> Sum.ttype sum
  | Tuple r | Record r -> Record.ttype r
  | Lazy (t, _) -> lazyt t
  | Prop (props, t, _) -> Mlfi_types.add_props_ttype props t
  | Object o -> Object.ttype o
  | Abstract (_, t, _) -> t

type Mlfi_types.memoized_type_prop += Xtype of Obj.t xtype


let rec search a n i =
  if i = n then (Obj.magic Unit)
  else match a.(i) with
    | Xtype r -> r
    | _ -> search a n (i + 1)

let find_memoized_xtype node : 'a xtype =
  Obj.magic (search node.Mlfi_types.rec_memoized (Array.length node.Mlfi_types.rec_memoized) 0)

let add_memoized_xtype node xt =
  let s = Xtype (Obj.magic xt) in
  let old = node.Mlfi_types.rec_memoized in
  let a = Array.make (Array.length old + 1) s in
  Array.blit old 0 a 1 (Array.length old);
  Mlfi_types.Internal.set_memoized node a;
  xt

let rec xtype_of_ttype (type s_) (s: s_ ttype) : s_ xtype =
  (* This function is used quite a lot (e.g. in the "cst" combinator),
     so we accept some internal unsafety to improve performance. *)
  match Mlfi_types.stype_of_ttype s with
  | DT_node{rec_name="unit"; _} -> Obj.magic Unit
  | DT_node{rec_name="bool"; _} -> Obj.magic Bool
  | DT_int -> Obj.magic Int
  | DT_float -> Obj.magic Float
  | DT_string -> Obj.magic String
  | DT_date -> Obj.magic Date
  | DT_list t -> Obj.magic (List (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_array t -> Obj.magic (Array (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_option t -> Obj.magic (Option (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_arrow (l, t1, t2) -> Obj.magic (Function (l, (cast_ttype t1, lazy (xtype_of_ttype (cast_ttype t1))),
                                                 (cast_ttype t2, lazy (xtype_of_ttype (cast_ttype t2)))))
  | DT_prop (p, t) -> Obj.magic (Prop (p, cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_abstract ("lazy_t", [t]) -> Obj.magic (Lazy (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_abstract ("char", []) -> Obj.magic Char
  | DT_abstract ("int32", []) -> Obj.magic Int32
  | DT_abstract ("int64", []) -> Obj.magic Int64
  | DT_abstract ("nativeint", []) -> Obj.magic Nativeint
  | DT_abstract ("floatarray", []) -> Obj.magic Floatarray
  | DT_tuple tl -> Tuple (build_tuple s tl)
  | DT_node({rec_descr=DT_record{record_repr; record_fields}; _} as node) ->
      begin match find_memoized_xtype node with
      | Unit -> add_memoized_xtype node (Record (build_record s record_repr record_fields))
      | r -> Obj.magic r
      end

  | DT_node({rec_descr=DT_variant{variant_constrs; variant_repr}; _} as node) ->
      begin match find_memoized_xtype node with
      | Unit -> add_memoized_xtype node (Sum (build_sum s variant_repr variant_constrs))
      | r -> Obj.magic r
      end

  | DT_object methods ->
      Object (build_object s methods)

  | DT_polyvariant _ ->
      failwith "Mlfi_xtypes: poly variant types not supported"

  | DT_abstract (name, args) -> Abstract (name, s, args)

  | DT_var _ ->
      assert false

let xtype_of_field (r : (_, 'a) RecordField.t) : 'a xtype =
  if r.xtype != dummy_xtype then
    Obj.magic r.xtype
  else begin
    let xt = xtype_of_ttype r.t in
    r.xtype <- Obj.repr xt;
    xt
  end

let xtype_of_constructor (r : (_, 'a) Constructor.t) : 'a xtype =
  if r.xtype != dummy_xtype then
    Obj.magic r.xtype
  else begin
    let xt = xtype_of_ttype r.t in
    r.xtype <- Obj.repr xt;
    xt
  end

let xtype_of_method (r : (_, 'a) Method.t) : 'a xtype =
  if r.xtype != dummy_xtype then
    Obj.magic r.xtype
  else begin
    let xt = xtype_of_ttype r.t in
    r.xtype <- Obj.repr xt;
    xt
  end

let get_first_props_xtype xt =
  let rec loop accu = function
    | Prop (l, _, lazy xt) ->
        loop (l :: accu) xt
    | xt ->
        List.concat (List.rev accu), xt
  in
  loop [] xt

let get_first_props_ttype t =
  let rec loop accu = function
    | Mlfi_types.DT_prop (l, t) ->
        loop (l :: accu) t
    | stype ->
        List.concat (List.rev accu), Obj.magic stype
  in
  loop [] (Mlfi_types.stype_of_ttype t)

let rec remove_first_props_xtype : type t. t xtype -> t xtype = function
  | Prop (_, _, lazy xt) -> remove_first_props_xtype xt
  | xt -> xt

type sttype = Ttype: 'a ttype -> sttype

let sttype_of_stype s = Ttype (Obj.magic s)

let rec all_paths: type root target. root:root ttype -> target:target ttype -> (root, target, _) Mlfi_type_path.t list = fun ~root ~target ->
  match Mlfi_types.ttypes_equality root target with
  | Some Mlfi_types.TypEq.Eq -> [[%p]]
  | None ->
      match xtype_of_ttype root with
      | Unit -> []
      | Bool -> []
      | Int -> []
      | Float -> []
      | String -> []
      | Date -> []
      | Char -> []
      | Int32 -> []
      | Int64 -> []
      | Nativeint -> []
      | Option (t, _) ->
          let paths = all_paths ~root:t ~target in
          List.map (Mlfi_type_path.(^^) [%p (Some)]) paths
      | Tuple record
      | Record record ->
          List.concat_map
            (function (Field f) ->
               let paths = all_paths ~target ~root:(RecordField.ttype f) in
               List.map (Mlfi_type_path.(^^) (RecordField.path f)) paths)
            (Record.fields record)
      | Sum sum ->
          List.concat
            (Mlfi_array.map_to_list
               (function (Constructor c) ->
                  let paths = all_paths ~target ~root:(Constructor.ttype c) in
                  List.map (Mlfi_type_path.(^^) (Constructor.path c)) paths)
               (Sum.constructors sum))
      | Prop (_, t, _) -> all_paths ~target ~root:t
      | Object _ -> []
      | List _ -> []
      | Array _ -> []
      | Floatarray -> []
      | Function _ -> []
      | Lazy _ -> []
      | Abstract _ -> []

let rec all_paths_value: type root target. root:root ttype -> target:target ttype -> root -> (root, target, _) Mlfi_type_path.t list =
  fun ~root ~target x ->
  match Mlfi_types.ttypes_equality root target with
  | Some Mlfi_types.TypEq.Eq -> [[%p]]
  | None ->
      match xtype_of_ttype root with
      | Unit -> []
      | Bool -> []
      | Int -> []
      | Float -> []
      | String -> []
      | Date -> []
      | Char -> []
      | Int32 -> []
      | Int64 -> []
      | Nativeint -> []
      | Option (t, _) ->
          begin match x with
          | None -> []
          | Some x ->
              let paths = all_paths_value ~root:t ~target x in
              List.map (Mlfi_type_path.(^^) [%p (Some)]) paths
          end
      | Tuple record
      | Record record ->
          List.concat_map
            (function (Field f) ->
               let paths = all_paths_value ~target ~root:(RecordField.ttype f) (RecordField.get f x) in
               List.map (Mlfi_type_path.(^^) (RecordField.path f)) paths)
            (Record.fields record)
      | Sum sum ->
          let Constructor c = Sum.constructor sum x in
          let paths = all_paths_value ~target ~root:(Constructor.ttype c) (Constructor.project_exn c x) in
          List.map (Mlfi_type_path.(^^) (Constructor.path c)) paths
      | Prop (_, t, _) -> all_paths_value ~target ~root:t x
      | List (t, _) ->
          List.concat
            (List.mapi
               (fun i x ->
                  let paths = all_paths_value ~root:t ~target x in
                  List.map (Mlfi_type_path.(^^) (Mlfi_type_path.type_path_get_list_nth i)) paths)
               x)
      | Array (t, _) ->
          Array.to_list
            (Array.mapi
               (fun i x ->
                  let paths = all_paths_value ~root:t ~target x in
                  List.map (Mlfi_type_path.(^^) (Mlfi_type_path.type_path_get_array_nth i)) paths)
               x)
          |> List.concat
      | Floatarray -> []
      | Function _ -> []
      | Lazy _ -> []
      | Object _ -> []
      | Abstract _ -> []

let is_record s_ttype : _ Record.t option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_node {rec_descr=DT_record _; _} ->
      begin match xtype_of_ttype s_ttype with
      | Record r -> Some r
      | _ -> assert false
      end
  | _ ->
      None

let is_sum (type s_) (s_ttype: s_ ttype) : s_ Sum.t option =
  match Mlfi_types.stype_of_ttype s_ttype with
  | DT_node{rec_name="unit"; _} ->
      None
  | DT_node{rec_descr=DT_variant _; _} ->
      begin match xtype_of_ttype s_ttype with
      | Sum r -> Some r
      | _ -> assert false
      end
  | _ ->
      None

let constructor_name ~t x =
  match is_sum t with
  | None -> invalid_arg __FUNCTION__
  | Some sum ->
      let Constructor c = Sum.constructor sum x in
      Constructor.name c

let smallest_size t =
  let rec aux seen = function
    | Mlfi_types.DT_tuple tl -> List.fold_left (fun acc t -> acc + aux seen t) 1 tl
    | DT_int | DT_float | DT_string | DT_date
    | DT_option _ | DT_list _ | DT_array _
    | DT_abstract _ (* ?? *)
      -> 1
    | DT_prop (_, t) -> aux seen t
    | DT_var _ | DT_object _ | DT_polyvariant _ | DT_arrow _ -> raise_notrace Exit
    | DT_node n when Mlfi_sets_maps.IntSet.mem n.Mlfi_types.rec_uid seen -> raise_notrace Exit
    | DT_node n ->
        let seen = Mlfi_sets_maps.IntSet.add n.rec_uid seen in
        match n.rec_descr with
        | DT_variant {variant_constrs = l; _} ->
            let n = min_constr seen l in
            if n < max_int then n else raise_notrace Exit
        | DT_record  {record_fields = l; _} ->
            List.fold_left (fun acc (_, _, t) -> acc + aux seen t) 1 l
  and min_constr seen l =
    List.fold_left
      (fun acc (_, _, args) ->
         try
           let n = List.fold_left (fun acc t -> acc + aux seen t) 1 (Mlfi_types.uninline args) in
           Int.min n acc
         with Exit ->
           acc
      )
      max_int
      l
  in
  try aux Mlfi_sets_maps.IntSet.empty t
  with Exit -> max_int

let default_value_reaches_node {Mlfi_types.rec_uid; _} s =
  let tbl = Hashtbl.create 8 in
  let rec aux = function
    | Mlfi_types.DT_tuple tl -> List.iter aux tl
    | DT_int | DT_float | DT_string | DT_date
    | DT_option _ | DT_list _ | DT_array _
    | DT_abstract _ (* ?? *)
    | DT_var _ | DT_object _ | DT_polyvariant _ | DT_arrow _ -> ()
    | DT_prop (_, t) -> aux t
    | DT_node n when n.Mlfi_types.rec_uid = rec_uid -> raise_notrace Exit
    | DT_node n when Hashtbl.mem tbl n.rec_uid -> ()
    | DT_node n ->
        Hashtbl.add tbl n.rec_uid ();
        match n.rec_descr with
        | DT_variant {variant_constrs = (_, _, t) :: _; _} -> List.iter aux (Mlfi_types.uninline t)
        | DT_variant {variant_constrs = []; _} -> raise_notrace Exit
        | DT_record  {record_fields = l; _} -> List.iter (fun (_, _, t) -> aux t) l
  in
  try aux s; false
  with Exit -> true

let enumerate t =
  match is_sum t with
  | None -> invalid_arg __FUNCTION__
  | Some sum ->
      let f (Constructor c) =
        match Mlfi_types.ttypes_equality_modulo_props (Constructor.ttype c) [%t: unit] with
        | Some Eq -> Constructor.inject c ()
        | None -> invalid_arg __FUNCTION__
      in
      Array.to_list (Array.map f (Sum.constructors sum))

module NestedFunction = struct
  type _ t =
    | Res: 'a ttype -> 'a t (* 'a is NOT a function *)
    | Fun: string * 'b ttype * 'c t -> ('b -> 'c) t

  let rec get(t : 'a ttype) : 'a t =
    match Mlfi_types.stype_of_ttype t with
    | DT_arrow (s, t1, t2) -> Obj.magic (Fun (s, cast_ttype t1, get (cast_ttype t2)))
    | _ -> Res t

end

let rec get_root_prop p = function
  | Mlfi_types.DT_prop (props, ty) ->
      begin match List.assoc_opt p props with
      | Some _ as r -> r
      | None -> get_root_prop p ty
      end
  | _ -> None

module ObjTbl = Hashtbl.Make (struct
    type t = Obj.t
    let equal (x : Obj.t) y = x == y
    let hash x = Hashtbl.hash_param 1 1 x
  end)

let pp_dot_string ppf s =
  Format.pp_print_char ppf '"';
  String.iter (function
      | '"' -> Format.pp_print_string ppf "\\\""
      | '\\' -> Format.pp_print_string ppf "\\\\"
      | '\n' -> Format.pp_print_string ppf "\\n"
      | '\r' -> Format.pp_print_string ppf "\\r"
      | '\t' -> Format.pp_print_string ppf "\\t"
      | c ->
          if Char.code c < 32 then Format.fprintf ppf "\\%03o" (Char.code c)
          else Format.pp_print_char ppf c
    ) s;
  Format.pp_print_char ppf '"'

type decision =
  | Default
  | Prune
  | Replace : 'b ttype * 'b -> decision

type override = { override: 'a. 'a ttype -> 'a -> decision }

let default_override = { override = (fun _ _ -> Default) }

let to_dot: type a. ?override:override -> t:a ttype -> Format.formatter -> a -> unit =
  fun ?(override = default_override) ~t ppf x ->
  let ids = ObjTbl.create 32 in
  let next_id = ref 0 in
  let fresh_id () = let id = !next_id in incr next_id; id in
  let node id label =
    Format.fprintf ppf "  n%d [label=%a];@\n" id pp_dot_string label
  in
  let edge ?label from_id to_id =
    match label with
    | None ->
        Format.fprintf ppf "  n%d -> n%d;@\n" from_id to_id
    | Some label ->
        Format.fprintf ppf "  n%d -> n%d [label=%a];@\n" from_id to_id pp_dot_string label
  in
  let edge' ?label from_id = function
    | None -> ()
    | Some to_id -> edge ?label from_id to_id
  in
  let type_name t =
    Format.asprintf "%a" Mlfi_types.print_stype (Mlfi_types.stype_of_ttype t)
  in
  let rec emit: type b. b ttype -> b -> int option = fun t x ->
    match override.override t x with
    | Prune -> None
    | Replace (t, x) -> emit t x
    | Default ->
        let t = Mlfi_types.remove_first_props_ttype t in
        if Obj.is_block (Obj.repr x) then
          match ObjTbl.find_opt ids (Obj.repr x) with
          | Some _ as id -> id
          | None -> Some (emit_new t x)
        else
          Some (emit_new t x)
  and emit_new: type b. b ttype -> b -> int = fun t x ->
    let id = fresh_id () in
    if Obj.is_block (Obj.repr x) then ObjTbl.add ids (Obj.repr x) id;
    begin match xtype_of_ttype t with
    | Unit -> node id "()"
    | Bool -> node id (string_of_bool x)
    | Int -> node id (string_of_int x)
    | Float -> node id (string_of_float x)
    | String -> node id (Printf.sprintf "%S" x)
    | Date -> node id (Mlfi_date.to_string x)
    | Char -> node id (Printf.sprintf "%C" x)
    | Int32 -> node id (Int32.to_string x)
    | Int64 -> node id (Int64.to_string x)
    | Nativeint -> node id (Nativeint.to_string x)
    | Object _ -> node id "<object>"
    | Option (t, _) ->
        begin match x with
        | None ->
            node id "None"
        | Some x ->
            node id "Some";
            edge' id (emit t x)
        end
    | List (t', _) ->
        begin match x with
        | [] ->
            node id "[]"
        | hd :: tl ->
            node id "::";
            edge' id (emit t' hd);
            edge' id (emit t tl)
        end
    | Array (t, _) ->
        node id (Printf.sprintf "<array:%d>" (Array.length x));
        Array.iter (fun x -> edge' id (emit t x)) x
    | Floatarray ->
        node id (Printf.sprintf "<floatarray:%d>" (Float.Array.length x));
        Float.Array.iter (fun x -> edge' id (emit [%t: float] x)) x
    | Function _ ->
        node id (Printf.sprintf "<function:%s>" (type_name t))
    | Sum sum ->
        let Constructor c = Sum.constructor sum x in
        let name = Constructor.name c in
        node id name;
        let arg = Constructor.project_exn c x in
        let arg_t = Constructor.ttype c in
        begin match xtype_of_ttype arg_t with
        | Unit -> ()
        | Record record | Tuple record -> emit_record_fields id record arg
        | _ -> edge' id (emit arg_t arg)
        end
    | Tuple record ->
        node id (Printf.sprintf "<tuple:%d>" (List.length (Record.fields record)));
        emit_record_fields id record x
    | Record record ->
        let name = List.hd (List.rev (String.split_on_char '.' (Record.path record))) in
        node id name;
        emit_record_fields id record x
    | Lazy (t, _) ->
        node id "lazy";
        begin match Lazy.force x with
        | x ->
            edge' id (emit t x)
        | exception exn ->
            let exn_id = fresh_id () in
            node exn_id (Printf.sprintf "<raised:%s>" (Printexc.to_string exn));
            edge id exn_id
        end
    | Prop _ ->
        assert false
    | Abstract (name, _, _) ->
        node id (Printf.sprintf "<abstract:%s>" name)
    end;
    id
  and emit_record_fields: type b. int -> b Record.t -> b -> unit = fun id record x ->
    List.iter (fun (Field field) ->
        edge' id (emit (RecordField.ttype field) (RecordField.get field x))
      ) (Record.fields record)
  in
  Format.fprintf ppf "@[<v>digraph mlfi_value {@\n";
  Format.fprintf ppf "  node [shape=box];@\n";
  ignore (emit t x);
  Format.fprintf ppf "}@]"

let rec has_subterm: type t u. t:t ttype -> u:u ttype -> (u -> bool) -> t -> bool = fun ~t ~u pred x ->
  match Mlfi_types.ttypes_equality_modulo_props t u with
  | None ->
      begin match xtype_of_ttype t with
      | Unit
      | Bool
      | Int
      | Float
      | String
      | Date
      | Char
      | Int32
      | Int64
      | Nativeint -> false
      | Option (t, _) ->
          begin match x with
          | None -> false
          | Some x -> has_subterm ~t ~u pred x
          end
      | Floatarray ->
          Float.Array.exists (has_subterm ~t:[%t:float] ~u pred) x
      | List (t, _) ->
          List.exists (has_subterm ~t ~u pred) x
      | Array (t, _) ->
          Array.exists (has_subterm ~t ~u pred) x
      | Lazy (t, _) -> has_subterm ~t ~u pred (Lazy.force x)
      | Prop (_, t, _) -> has_subterm ~t ~u pred x
      | Sum sum ->
          let Constructor c = Sum.constructor sum x in
          has_subterm ~t:(Constructor.ttype c) ~u pred (Constructor.project_exn c x)
      | Record record
      | Tuple record ->
          let fields = Record.fields record in
          List.exists (function (Field f) -> has_subterm ~t:(RecordField.ttype f) ~u pred (RecordField.get f x)) fields
      | Function _ -> false
      | Object _ -> false
      | Abstract _ -> false
      end
  | Some Mlfi_types.TypEq.Eq -> pred x
