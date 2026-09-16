(***************************************************************************)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* Inspired by Bryan O'Sullivan's aeson library. *)


open Mlfi_xtypes

type error = string

let string_of_json_path path =
  List.rev path |> String.concat "."

let string_of_ttype t =
  Format.asprintf "%a" Mlfi_types.print_stype (Mlfi_types.stype_of_ttype t)

type number = I of int | F of float

module Number : sig
  type t
  val to_float: t -> float
  val round_to_int: t -> int
  val to_int: t -> int option
  val to_string: t -> string

  val of_int: int -> t
  val of_float: float -> t (* does not check for nan/infinity *)

  val repr: t -> number
end = struct
  type t = number

  let repr x = x

  let of_int x = I x

  let of_float x = F x

  let to_float = function
    | I x -> float_of_int x
    | F x -> x

  let round_to_int = function
    | I x -> x
    | F x -> int_of_float x

  let to_int = function
    | I x -> Some x
    | F x ->
        let i = int_of_float x in
        if float_of_int i = x then Some i else None

  let to_string = function
    | I x -> string_of_int x
    | F x -> string_of_float x
end

type value =
  | Null
  | Bool of bool
  | Number of Number.t
  | String of string
  | Array of value list
  | Object of (string * value) list

exception Json_failure of string

let null = Null
let bool b = Bool b
let int n = Number (Number.of_int n)

let float x =
  match classify_float x with
  | FP_infinite when x < 0. -> String "-Infinity"
  | FP_infinite -> String "Infinity"
  | FP_nan -> String "NaN"
  | _ -> Number (Number.of_float x)

let string s = String s
let array l = Array l
let object_ l = Object l

let json_failure msg = raise (Json_failure msg)
let json_failuref fmt = Printf.ksprintf json_failure fmt

let protect f =
  try Ok (f ()) with
  | Json_failure msg -> Error msg

let invalid_arg_of_json_failure f =
  try f () with
  | Json_failure msg -> invalid_arg msg

let unwrap_of_json_result = function
  | Ok x -> x
  | Error msg -> json_failure msg

type ctx =
  {
    to_json_field: string -> string;
    lossy: bool;
    to_json: to_json_override option;
    of_json: of_json_override option;
  }

and to_json_override =
  {
    to_json: 'a. 'a ttype -> ('a -> value) option;
  }

and of_json_override =
  {
    of_json: 'a. 'a ttype -> (value -> ('a, error) result) option;
  }

let ctx ?(to_json_field=Fun.id) ?lossy ?to_json ?of_json () =
  {
    to_json_field;
    lossy = lossy <> None;
    to_json;
    of_json;
  }

let empty_ctx = ctx ()

let to_json_field ?allow_verbatim f =
  let drop_prefix ~prefix s =
    if String.starts_with ~prefix s then
      Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
    else
      None
  in
  if allow_verbatim <> None then
    fun s ->
      match drop_prefix ~prefix:"__" s with
      | Some s -> s
      | None -> f s
  else
    f

let allow_verbatim_ctx () =
  ctx ~to_json_field:(to_json_field ~allow_verbatim:() Fun.id) ()

let trim c s =
  let len = String.length s in
  let left = ref 0 in
  let right = ref (len - 1) in
  while !left <= !right && s.[ !left ] = c do
    incr left
  done;
  while !right >= !left && s.[ !right ] = c do
    decr right
  done;
  String.sub s !left (!right - !left + 1)

let caml_case_ctx ?allow_verbatim () =
  let to_json_field =
    to_json_field ?allow_verbatim
      (fun s ->
         trim '_' s
         |> String.split_on_char '_'
         |> List.mapi (fun i s -> if i > 0 then String.capitalize_ascii s else s)
         |> String.concat ""
      )
  in
  ctx ~to_json_field ()

let pascal_case_ctx ?allow_verbatim () =
  let to_json_field =
    to_json_field ?allow_verbatim
      (fun s ->
         trim '_' s
         |> String.split_on_char '_'
         |> List.map String.capitalize_ascii
         |> String.concat ""
      )
  in
  ctx ~to_json_field ()

let trim_fields_ctx () =
  ctx ~to_json_field:(trim '_') ()

type 'a t_proxy = 'a ttype * ('a -> value) * (value -> 'a)
type proxy = Proxy: 'a t_proxy -> proxy

let abs0_tbl = Hashtbl.create 16

let type_name t =
  match Mlfi_types.stype_of_ttype t with
  | DT_abstract (s, []) | DT_node {rec_name = s; _} -> Some s
  | _ -> None

let register_conversion ~t ~to_json ~of_json =
  let t = Mlfi_types.remove_first_props_ttype t in
  let name =
    match type_name t with
    | Some s -> s
    | _ -> invalid_arg __FUNCTION__
  in
  let proxy = Proxy (t, to_json, fun x -> unwrap_of_json_result (of_json x)) in
  Hashtbl.add abs0_tbl name proxy

let find_proxy (type t) (t : t ttype) =
  match type_name t with
  | None -> None
  | Some s ->
      Hashtbl.find_all abs0_tbl s
      |> List.find_map
        (fun (Proxy ((t', _, _) as p)) ->
           match Mlfi_types.ttypes_equality t t' with
           | Some Mlfi_types.TypEq.Eq -> Some (p : t t_proxy)
           | None -> None
        )

module type ABSTRACT_1_CONVERSION =
sig
  type 'a t
  val t: unit t ttype
  val to_json: t:'a ttype -> ?ctx:ctx -> 'a t -> value
  val of_json: t:'a ttype -> ?ctx:ctx -> value -> ('a t, error) result
end

module type ABS1 =
sig
  type 'a t
  include ABSTRACT_1_MATCHER_SIG with type 'a t := 'a t
  val to_json: t:'a ttype -> ?ctx:ctx -> 'a t -> value
  val of_json: t:'a ttype -> ?ctx:ctx -> value -> 'a t
end

let abs1_tbl = Hashtbl.create 16

let register_parametric_conversion (module T : ABSTRACT_1_CONVERSION) =
  let module M = struct
    include ABSTRACT_1_MATCHER(T)
    let to_json  = T.to_json
    let of_json ~t ?ctx x = unwrap_of_json_result (T.of_json ~t ?ctx x)
  end
  in
  Hashtbl.add abs1_tbl M.name (module M : ABS1)


module Reader: sig
  type 'a t
  val mk: (string * 'a) list -> 'a t
  val get: 'a t -> string -> 'a -> 'a
end = struct
  type 'a t = (string * 'a) list ref

  let mk l = ref l

  (* Optimized lookup for record fields, for the case where the ordering
     match between the JSON value and the OCaml record definition. *)

  let get (r : 'a t) s default =
    match !r with
    | (t, v) :: rest when t = s -> r := rest; v
    | [] -> default
    | _ :: rest ->
        (* could fallback to building a lookup table here... *)
        match List.assoc_opt s rest with
        | None -> default
        | Some x -> x
end


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

let is_json_attribute t = Mlfi_types.ttypes_equality [%t: string * value] t

let as_json_string props =
  List.mem_assoc "as_json_string" props

let rec list_iter_comma handlers f = function
  | [] -> ()
  | hd :: tl -> handlers.comma (); f handlers hd; list_iter_comma handlers f tl

let list_iter_comma handlers f = function
  | [] -> ()
  | hd :: tl -> f handlers hd; list_iter_comma handlers f tl

let float_to_json_stream handlers x =
  match classify_float x with
  | FP_infinite when x < 0. -> handlers.string "-Infinity"
  | FP_infinite -> handlers.string "Infinity"
  | FP_nan -> handlers.string "NaN"
  | _ -> handlers.float x

let rec to_json_stream_internal: type t. ctx -> t xtype -> (json_signal_handlers -> t -> unit) = fun ctx t ->
  let tt = ttype_of_xtype t in
  match ctx.to_json with
  | Some { to_json } ->
      begin match to_json tt with
      | Some f -> fun handlers x -> handlers.value (f x)
      | None -> to_json_stream_structural ctx t
      end
  | None -> to_json_stream_structural ctx t

and to_json_stream_structural: type t. ctx -> t xtype -> (json_signal_handlers -> t -> unit) = fun ctx t ->
  let props, t = get_first_props_xtype t in
  match t with
  | Unit -> (fun handlers _ -> handlers.object_open (); handlers.object_close ())
  | Prop (_, _, lazy xt) -> to_json_stream_internal ctx xt
  | Bool -> (fun handlers x -> handlers.bool x)
  | Int -> (fun handlers x -> handlers.int x)
  | Float -> float_to_json_stream
  | String -> (fun handlers x -> handlers.string x)
  | Option (_, lazy xt) ->
      begin match remove_first_props_xtype xt with
      | Option (_, lazy xt) ->
          let xt_fn = lazy (to_json_stream_internal ctx xt) in
          (fun handlers -> function
             | None ->
                 handlers.object_open ();
                 handlers.object_field "type";
                 handlers.string "None";
                 handlers.object_close ()
             | Some None ->
                 handlers.object_open ();
                 handlers.object_field "type";
                 handlers.string "Some";
                 handlers.object_close ()
             |  Some (Some x) ->
                 handlers.object_open ();
                 handlers.object_field "type";
                 handlers.string "Some";
                 handlers.comma ();
                 handlers.object_field "val";
                 Lazy.force xt_fn handlers x;
                 handlers.object_close ()
          )
      | _ ->
          let xt_fn = lazy (to_json_stream_internal ctx xt) in
          (fun handlers -> function
             | None -> handlers.null ()
             | Some x -> Lazy.force xt_fn handlers x
          )
      end
  | List (tt, lazy xt) ->
      let default () =
        let xt_fn = lazy (to_json_stream_internal ctx xt) in
        let f handlers item = Lazy.force xt_fn handlers item in
        (fun handlers x ->
           handlers.array_open ();
           list_iter_comma handlers f x;
           handlers.array_close ()
        )
      in
      if ctx.lossy then
        begin match is_json_attribute tt with
        | Some Eq ->
            (fun handlers x ->
               handlers.object_open ();
               list_iter_comma handlers (fun handlers (key, value) ->
                   handlers.object_field key;
                   handlers.value value
                 ) x;
               handlers.object_close ()
            )
        | None ->
            default ()
        end
      else default ()
  | Array (_, lazy xt) ->
      let xt_fn = lazy (to_json_stream_internal ctx xt) in
      (fun handlers x ->
         handlers.array_open ();
         for i = 0 to Array.length x - 1 do
           if i > 0 then handlers.comma ();
           Lazy.force xt_fn handlers x.(i)
         done;
         handlers.array_close ()
      )
  | Floatarray ->
      (fun handlers x ->
         handlers.array_open ();
         for i = 0 to Float.Array.length x - 1 do
           if i > 0 then handlers.comma ();
           float_to_json_stream handlers (Float.Array.get x i)
         done;
         handlers.array_close ()
      )
  | Tuple record ->
      let rec prepare first = function
        | [] -> assert (not first); (fun handlers _ -> handlers.array_close ())
        | (Field hd) :: tl ->
            let field_xtype = xtype_of_field hd in
            let field_fn = to_json_stream_internal ctx field_xtype in
            let tl = prepare false tl in
            if first then
              (fun handlers x ->
                 handlers.array_open ();
                 field_fn handlers (RecordField.get hd x);
                 tl handlers x
              )
            else
              (fun handlers x ->
                 handlers.comma ();
                 field_fn handlers (RecordField.get hd x);
                 tl handlers x
              )
      in
      prepare true (Record.fields record)
  | Record record ->
      begin match find_proxy (Record.ttype record) with
      | Some (_, f, _) -> (fun handlers x -> handlers.value (f x))
      | None ->
          let record_fn = to_json_stream_record_curried_internal ctx (Record.fields record) in
          (fun handlers x ->
             handlers.object_open ();
             record_fn handlers true x;
             handlers.object_close ()
          )
      end
  | Sum sum ->
      begin match Sum.path sum with
      | "Mlfi_json.value" ->
          let Mlfi_types.TypEq.Eq = Option.get (Mlfi_types.ttypes_equality (Sum.ttype sum) [%t: value]) in
          (fun handlers x -> handlers.value x)
      | _ ->
          match find_proxy (Sum.ttype sum) with
          | Some (_, f, _) -> fun handlers x -> handlers.value (f x)
          | None ->
              let prepare_constructor (Constructor c) =
                let name = Constructor.name c in
                if (as_json_string props || as_json_string (Constructor.props c)) &&
                   Option.is_some (Mlfi_types.ttypes_equality_modulo_props (Constructor.ttype c) [%t: unit])
                then
                  (fun handlers _ -> handlers.string name)
                else
                  let xt = xtype_of_constructor c in
                  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype (Constructor.ttype c)), xtype_of_constructor c with
                  | DT_node{rec_descr=DT_record _; _}, Record record ->
                      let record_fn = to_json_stream_record_curried_internal ctx (Record.fields record) in
                      (fun handlers x ->
                         let y = Constructor.project_exn c x in
                         handlers.object_open ();
                         handlers.object_field "type";
                         handlers.string name;
                         record_fn handlers false y;
                         handlers.object_close ()
                      )
                  | DT_node{rec_name="unit"; _}, _ ->
                      (fun handlers _ ->
                         handlers.object_open ();
                         handlers.object_field "type";
                         handlers.string name;
                         handlers.object_close ()
                      )
                  | _ ->
                      let xt_fn = to_json_stream_internal ctx xt in
                      (fun handlers x ->
                         let y = Constructor.project_exn c x in
                         handlers.object_open ();
                         handlers.object_field "type";
                         handlers.string name;
                         handlers.comma ();
                         handlers.object_field "val";
                         xt_fn handlers y;
                         handlers.object_close ()
                      )
              in
              let constructors = Array.map (fun c -> lazy (prepare_constructor c)) (Sum.constructors sum) in
              fun handlers x ->
                let i = Sum.get_constructor_index sum x in
                Lazy.force constructors.(i) handlers x
      end
  | Lazy (_, lazy xt) ->
      let xt_fn = to_json_stream_internal ctx xt in
      (fun handlers x -> xt_fn handlers (Lazy.force x))
  | Function _ -> (fun _ _ -> json_failure "Mlfi_json: functions not supported")
  | Char -> (fun handlers x -> handlers.string (String.make 1 x))
  | Int32 -> (fun handlers x -> handlers.string (Int32.to_string x))
  | Int64 -> (fun handlers x -> handlers.string (Int64.to_string x))
  | Nativeint -> (fun handlers x -> handlers.string (Nativeint.to_string x))
  | Object _ -> (fun _ _ -> json_failure "Mlfi_json: objects not supported")
  | Abstract (_, t, _) ->
      let stype = Mlfi_types.stype_of_ttype t in
      let default () = json_failure "Mlfi_json: unsupported abstract type" in
      begin match stype with
      | DT_abstract (_, []) ->
          begin match find_proxy t with
          | None -> default ()
          | Some (_, f, _) -> (fun handlers x -> handlers.value (f x))
          end
      | DT_abstract (s, [_]) ->
          begin match Hashtbl.find_opt abs1_tbl s with
          | None -> default ()
          | Some(module T : ABS1) ->
              begin match T.is_t t with
              | Some T.Is (t, Mlfi_types.TypEq.Eq) -> (fun handlers x -> handlers.value (T.to_json ~t ~ctx x))
              | None -> assert false
              end
          end
      | _ ->
          default ()
      end

and to_json_stream_record_curried_internal: type t. ctx -> t Mlfi_xtypes.has_record_field list -> (json_signal_handlers -> bool -> t -> unit) = fun ctx -> function
  | [] -> (fun _handlers _first _x -> ())
  | (Field r) :: tl ->
      let field_xtype = xtype_of_field r in
      let field_fn = to_json_stream_internal ctx field_xtype in
      let _props, t = get_first_props_xtype field_xtype in
      let tl = to_json_stream_record_curried_internal ctx tl in
      let name = ctx.to_json_field (RecordField.name r) in
      match t with
      | Option _ ->
          (fun handlers first x ->
             (* For option fields, check if the value is None before including field *)
             match RecordField.get r x with
             | None ->
                 tl handlers first x
             | Some _ as field_value ->
                 if not first then handlers.comma ();
                 handlers.object_field name;
                 field_fn handlers field_value;
                 tl handlers false x
          )
      | Sum sum when Sum.path sum = "Mlfi_json.value" ->
          let Mlfi_types.TypEq.Eq = Option.get (Mlfi_types.ttypes_equality (Sum.ttype sum) [%t: value]) in
          (fun handlers first x ->
             match RecordField.get r x with
             | Null -> tl handlers first x
             | v ->
                 if not first then handlers.comma ();
                 handlers.object_field name;
                 handlers.value v;
                 tl handlers false x
          )
      |  _ ->
          (fun handlers first x ->
             if not first then handlers.comma ();
             handlers.object_field name;
             field_fn handlers (RecordField.get r x);
             tl handlers false x
          )

let rec value_to_signal_stream handlers = function
  | Null -> handlers.null ()
  | Bool b -> handlers.bool b
  | Number n ->
      begin match Number.repr n with
      | I x -> handlers.int x
      | F x -> handlers.float x
      end
  | String s -> handlers.string s
  | Array l ->
      handlers.array_open ();
      list_iter_comma handlers (fun handlers item -> handlers.value item) l;
      handlers.array_close ()
  | Object l ->
      handlers.object_open ();
      list_iter_comma handlers (fun handlers (key, value) -> handlers.object_field key; value_to_signal_stream handlers value) l;
      handlers.object_close ()

let to_json_internal ?(ctx=empty_ctx) ~t x =
  let value_stack = Dynarray.create () in
  let field_stack = Dynarray.create () in
  let shape_stack = Dynarray.create () in (* for each nested array/record, keep the size of the value_stack at the beginning (at the end, it gives the number of fields/elements to pop) *)

  let value v = Dynarray.add_last value_stack v in

  let handlers = {
    object_open = (fun () -> Dynarray.add_last shape_stack (Dynarray.length value_stack));
    object_field = (fun key -> Dynarray.add_last field_stack key);
    object_close = (fun () ->
        let n = Dynarray.length value_stack - Dynarray.pop_last shape_stack in
        let acc = ref [] in
        for _ = 0 to n - 1 do
          acc := (Dynarray.pop_last field_stack, Dynarray.pop_last value_stack) :: !acc
        done;
        value (Object !acc)
      );
    array_open = (fun () -> Dynarray.add_last shape_stack (Dynarray.length value_stack));
    array_close = (fun () ->
        let n = Dynarray.length value_stack - Dynarray.pop_last shape_stack in
        let acc = ref [] in
        for _ = 0 to n - 1 do
          acc := Dynarray.pop_last value_stack :: !acc
        done;
        value (Array !acc)
      );
    comma = (fun () -> ()); (* No-op for tree building *)
    int = (fun i -> value (Number (Number.of_int i)));
    float = (fun f -> value (Number (Number.of_float f)));
    bool = (fun b -> value (Bool b));
    string = (fun s -> value (String s));
    null = (fun () -> value Null);
    value;
  }
  in
  to_json_stream_internal ctx (xtype_of_ttype t) handlers x;
  Dynarray.pop_last value_stack

let to_json_stream ctx t =
  fun handlers x ->
  invalid_arg_of_json_failure (fun () -> to_json_stream_internal ctx t handlers x)

let to_json ?(ctx=empty_ctx) ~t x =
  invalid_arg_of_json_failure (fun () -> to_json_internal ~ctx ~t x)

(* Code taken from js_of_ocaml. *)

let buffer_add_unicode_escape =
  let conv = "0123456789abcdef" in
  fun b c ->
    Buffer.add_char b '\\';
    Buffer.add_char b 'u';
    Buffer.add_char b conv.[(c lsr 12) land 0xf];
    Buffer.add_char b conv.[(c lsr 8) land 0xf];
    Buffer.add_char b conv.[(c lsr 4) land 0xf];
    Buffer.add_char b conv.[c land 0xf]

let js_escaping_to_buf ~escape_single_quote ~escape_html_tags b s =
  let l = String.length s in
  let i = ref 0 in
  let add_unicode_escape () =
    let c =
      let d = String.get_utf_8_uchar s !i in
      i := !i + Uchar.utf_decode_length d;
      decr i;
      Uchar.to_int (Uchar.utf_decode_uchar d)
    in
    if c <= 0xFFFF then
      buffer_add_unicode_escape b c
    else
      let c = c - 0x1_0000 in
      let high_surrogate = (c lsr 10) land 0b11_1111_1111 + 0xD800 in
      let low_surrogate = c land 0b11_1111_1111 + 0xDC00 in
      buffer_add_unicode_escape b high_surrogate;
      buffer_add_unicode_escape b low_surrogate;
  in
  while !i < l do
    begin match s.[!i] with
    | '\b' ->
        Buffer.add_string b "\\b"
    | '\t' ->
        Buffer.add_string b "\\t"
    | '\n' ->
        Buffer.add_string b "\\n"
    | '\012' ->
        Buffer.add_string b "\\f"
    | '\r' ->
        Buffer.add_string b "\\r"
    | '\"' ->
        Buffer.add_string b "\\\""
    | '\\' ->
        Buffer.add_string b "\\\\"
    | '\000' .. '\031' | '\127' .. '\255' ->
        add_unicode_escape ()
    | '\'' when escape_single_quote ->
        add_unicode_escape ()
    | '<' when escape_html_tags ->
        (*To avoid that the browser interprets the <script/> tag in js parameters.*)
        add_unicode_escape ()
    | '\032' .. '\126' as c ->
        Buffer.add_char b c (* could group consecutive bytes that don't need escaping and call add_string once *)
    end;
    incr i
  done

let rec check_no_escaping_needed_json s i =
  i = String.length s ||
  match s.[i] with | '\"' | '\\' -> false | '\032' .. '\126' -> check_no_escaping_needed_json s (i + 1) | _ -> false

let js_escaping ~escape_single_quote ~escape_html_tags s =
  let b = Buffer.create (4 * String.length s) in (* could be less pessimistic and/or reuse a global buffer *)
  js_escaping_to_buf ~escape_single_quote ~escape_html_tags b s;
  Buffer.contents b

let json_escaping s =
  if check_no_escaping_needed_json s 0 then s
  else js_escaping ~escape_single_quote:false ~escape_html_tags:false s

let json_escaping_to_buf buf s =
  if check_no_escaping_needed_json s 0 then Buffer.add_string buf s
  else js_escaping_to_buf ~escape_single_quote:false ~escape_html_tags:false buf s

let string_handlers output =
  let rec handlers = {
    object_open = (fun () -> output "{");
    object_field = (fun key -> output "\""; output (json_escaping key); output "\":");
    object_close = (fun () -> output "}");
    array_open = (fun () -> output "[");
    array_close = (fun () -> output "]");
    comma = (fun () -> output ",");
    int = (fun i -> output (string_of_int i));
    float = (fun f -> output (string_of_float f));
    bool = (fun b -> output (string_of_bool b));
    string = (fun s -> output "\""; output (json_escaping s); output "\"");
    null = (fun () -> output "null");
    value = (fun v -> value_to_signal_stream handlers v);
  }
  in
  handlers

let to_json_string_stream_internal ?(ctx=empty_ctx) ~t output x =
  to_json_stream_internal ctx (xtype_of_ttype t) (string_handlers output) x

let to_json_string_stream ?(ctx=empty_ctx) ~t output x =
  invalid_arg_of_json_failure (fun () -> to_json_string_stream_internal ~ctx ~t output x)

let buffer_handlers buf =
  let rec handlers = {
    object_open = (fun () -> Buffer.add_char buf '{');
    object_field = (fun key -> Buffer.add_char buf '\"'; json_escaping_to_buf buf key; Buffer.add_string buf "\":");
    object_close = (fun () -> Buffer.add_char buf '}');
    array_open = (fun () -> Buffer.add_char buf '[');
    array_close = (fun () -> Buffer.add_char buf ']');
    comma = (fun () -> Buffer.add_char buf ',');
    int = (fun i -> Buffer.add_string buf (string_of_int i));
    float = (fun f -> Buffer.add_string buf (string_of_float f));
    bool = (fun b -> Buffer.add_string buf (string_of_bool b));
    string = (fun s -> Buffer.add_char buf '\"'; json_escaping_to_buf buf s; Buffer.add_char buf '\"');
    null = (fun () -> Buffer.add_string buf "null");
    value = (fun v -> value_to_signal_stream handlers v);
  }
  in
  handlers

let encode x =
  let buf = Buffer.create 256 in
  value_to_signal_stream (buffer_handlers buf) x;
  Buffer.contents buf

let to_json_buffer_internal ctx t =
  let f = to_json_stream_internal ctx t in
  fun buf x -> f (buffer_handlers buf) x

let to_json_buffer ctx t =
  fun buf x -> invalid_arg_of_json_failure (fun () -> to_json_buffer_internal ctx t buf x)

let to_json_string_internal ?(ctx = empty_ctx) ~t =
  let f = to_json_buffer_internal ctx (xtype_of_ttype t) in
  fun x ->
    let buf = Buffer.create 256 in
    f buf x;
    Buffer.contents buf

let to_json_string ?(ctx = empty_ctx) ~t =
  fun x -> invalid_arg_of_json_failure (fun () -> to_json_string_internal ~ctx ~t x)

let of_json_error _t (_x : value) =
  json_failure "Type/value mismatch"

let of_json_internal ?(ctx=empty_ctx) ~t x =
  let rec of_json: type t. t: t ttype -> value -> t = fun ~t x ->
    of_json_xtype (xtype_of_ttype t) [] x

  and of_json_xtype: type t. t xtype -> string list -> value -> t = fun t path v ->
    let tt = ttype_of_xtype t in
    match ctx.of_json with
    | Some { of_json } ->
        begin match of_json tt with
        | Some f -> unwrap_of_json_result (f v)
        | None -> of_json_xtype_structural t path v
        end
    | None -> of_json_xtype_structural t path v

  and of_json_xtype_structural: type t. t xtype -> string list -> value -> t = fun t path v ->
    let props, t = get_first_props_xtype t in
    match t, v with
    | Unit, Object _ -> ()
    | Bool, Bool b -> b
    | Int, Number x -> Number.round_to_int x
    | Char, String s -> s.[0]
    | Int32, String s -> Int32.of_string s
    | Int64, String s -> Int64.of_string s
    | Nativeint, String s -> Nativeint.of_string s
    | Float, Number x -> Number.to_float x
    | Float, String "Infinity" -> infinity
    | Float, String "-Infinity" -> neg_infinity
    | Float, String "NaN" -> nan
    | String, String x -> x
    | Option _, Null -> None
    | Option (_, lazy xt), x ->
        begin match remove_first_props_xtype xt with
        | Option (_, lazy xt) ->
            begin match x with
            | Object l ->
                let constr, arg =
                  match l with
                  | [ "type", String ty; "val", v ] -> ty, Some v
                  | [ "type", String ty ] -> ty, None
                  | _ -> get_constr l, List.assoc_opt "val" l
                in
                begin match constr with
                | "None" -> None
                | "Some" -> Some (match arg with None -> None | Some arg -> Some (of_json_xtype xt ("(Some)":: path) arg))
                | _ ->
                    json_failure
                      "Nested option, 'type' field should be Some or None"
                end
            | _ ->
                json_failure
                  "Type/value mismatch"
            end
        | _ ->
            Some (of_json_xtype xt ("(Some)" :: path) x)
        end

    | List (_, lazy xt), Array l -> List.map (of_json_xtype xt ("(_)" :: path)) l
    | Array (_, lazy xt), Array l -> Array.of_list (List.map (of_json_xtype xt ("[_]" :: path)) l)
    | Floatarray, Array l ->
        let x = Float.Array.create (List.length l) in
        List.iteri (fun i e -> Float.Array.set x i (of_json_xtype Float ("[[_]]" :: path) e)) l;
        x
    | Tuple record, Array l ->
        (* TODO: check that !l is empty at the end? *)
        let l = ref l in
        Record.build record
          {mk = fun r ->
              match !l with
              | hd :: tl -> l := tl; of_json_xtype (xtype_of_field r) ("(_/_)" :: path) hd
              | _ -> json_failure "Length mismatch"
          }
    | Record record, _ ->
        begin match find_proxy (Record.ttype record) with
        | Some (_, _, f) -> f v
        | None ->
            match v with
            | Object l ->
                let tbl = Reader.mk l in
                Record.build record {mk = fun r ->
                    let name = RecordField.name r in
                    of_json_xtype (xtype_of_field r) (name :: path) (Reader.get tbl (ctx.to_json_field name) Null)
                  }
            | _ -> of_json_error (Record.ttype record) v
        end
    | Sum sum, _ ->
        begin match Sum.path sum with
        | "Mlfi_json.value" ->
            let Mlfi_types.TypEq.Eq = Option.get (Mlfi_types.ttypes_equality (Sum.ttype sum) [%t: value]) in
            v
        | _ ->
            begin match find_proxy (Sum.ttype sum) with
            | Some (_, _, f) -> f v
            | None ->
                match v with
                | Object l ->
                    let constr, arg =
                      match l with
                      | [ "type", String ty; "val", v ] -> ty, v
                      | ("type", String ty) :: rest -> ty, Object rest
                      | _ ->
                          get_constr l,
                          match List.assoc_opt "val" l with
                          | None -> Object (List.remove_assoc "type" l)
                          | Some arg -> arg
                    in
                    let i = Sum.lookup_constructor sum constr in
                    if i < 0 then json_failuref "Unexpected constructor %S" constr;
                    let (Constructor c) = (Sum.constructors sum).(i) in
                    Constructor.inject c
                      (of_json_xtype (xtype_of_constructor c) (("(" ^ constr ^ ")") :: path) arg)
                | String constr ->
                    let i = Sum.lookup_constructor sum constr in
                    if i < 0 then of_json_error (Sum.ttype sum) v;
                    let Constructor c = (Sum.constructors sum).(i) in
                    begin match Mlfi_types.ttypes_equality_modulo_props (Constructor.ttype c) [%t: unit] with
                    | Some Mlfi_types.TypEq.Eq when as_json_string props || as_json_string (Constructor.props c) ->
                        Constructor.inject c ()
                    | _ -> of_json_error (Sum.ttype sum) v
                    end
                | _ -> of_json_error (Sum.ttype sum) v
            end
        end
    | Lazy (_, lazy xt), _ -> lazy (of_json_xtype xt ("lazy" :: path) v)
    | Prop (_, _, lazy xt), _ -> of_json_xtype xt path v
    | Function _, _ -> json_failure "Mlfi_json: functions not supported"
    | Abstract (_, t, _), x ->
        let stype = Mlfi_types.stype_of_ttype t in
        begin match stype with
        | DT_abstract (_, []) ->
            begin match find_proxy t with
            | None -> json_failure "Unexpected abstract type"
            | Some (_, _, f) -> f x
            end
        | DT_abstract (s, [_]) ->
            begin match Hashtbl.find_opt abs1_tbl s with
            | None -> json_failure "Unexpected abstract type"
            | Some(module T : ABS1) ->
                begin match T.is_t t with
                | Some T.Is (t, Mlfi_types.TypEq.Eq) ->
                    (T.of_json ~t ~ctx x : t)
                | None -> assert false
                end
            end
        | _ ->
            json_failure "Unexpected abstract type"
        end
    | _ ->
        json_failure
          "Type/value mismatch"

  and get_constr l =
    match List.assoc_opt "type" l with
    | Some(String constr) -> constr
    | None ->
        json_failure "No 'type' field in object of sum type"
    | Some _ ->
        json_failure "'type' field is not a string"

  in
  of_json ~t x

let of_json ?(ctx=empty_ctx) ~t x =
  protect (fun () -> of_json_internal ~ctx ~t x)

let buffer_add_cp b cp =
  Buffer.add_utf_8_uchar b (Uchar.of_int cp)

let in_high_surrogate_range cp = 0xD800 <= cp && cp <= 0xDBFF
let in_low_surrogate_range cp = 0xDC00 <= cp && cp <= 0xDFFF

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let hex = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> 10 + (Char.code c - Char.code 'a')
  | 'A'..'F' as c -> 10 + (Char.code c - Char.code 'A')
  | _ -> raise_notrace Exit

let pos s i =
  let line = ref 1 and bol = ref 0 in
  for i = 0 to Int.min (String.length s) i - 1 do
    if s.[i] = '\n' || (s.[i] = '\r' && (i+1 < String.length s && s.[i+1] <> '\n')) then (incr line; bol := i)
  done;
  (!line, i - !bol)

module type JsonBuilder = sig
  type t
  val atom: int -> int -> value -> t
  val array: int -> int -> t list -> t
  val object_: int -> int -> (string * t) list -> t (* also keep locs for each field, incl their label? *)
end

module JsonBuilder_noloc = struct
  type t = value
  let atom _ _ x = x
  let array _ _ x = Array x
  let object_ _ _ x = Object x
end

let json_parser (type t) (module J : JsonBuilder with type t = t) ?filename ~check_eof s i : t * int =
  let n = String.length s in
  let i = ref i in
  let digits () = incr i; while !i < n && is_digit (String.unsafe_get s !i) do incr i done in
  let get () = if !i >= n then raise_notrace Exit else String.unsafe_get s !i in
  let rec next () = let c = get () in if is_space c then (incr i; next ()) else c in
  let b = Buffer.create 8 in

  let rec value () : t =
    let i0 = !i in
    match get () with
    | '{' ->
        incr i;
        (match next () with '}' -> incr i; J.object_ i0 !i [] | c -> obj i0 c [])
    | '[' ->
        incr i;
        if next () = ']' then (incr i; J.array i0 !i []) else array i0 []
    | '"' -> incr i; let s = string () in J.atom i0 !i (String s)
    | 't' ->
        if !i+3<n
        && String.unsafe_get s (!i+1) = 'r'
        && String.unsafe_get s (!i+2) = 'u'
        && String.unsafe_get s (!i+3) = 'e' then (i := !i + 4; J.atom i0 !i (Bool true))
        else raise_notrace Exit
    | 'f' ->
        if !i+4<n
        && String.unsafe_get s (!i+1) = 'a'
        && String.unsafe_get s (!i+2) = 'l'
        && String.unsafe_get s (!i+3) = 's'
        && String.unsafe_get s (!i+4) = 'e'
        then (i := !i + 5; J.atom i0 !i (Bool false))
        else raise_notrace Exit
    | 'n' ->
        if !i+3<n
        && String.unsafe_get s (!i+1) = 'u'
        && String.unsafe_get s (!i+2) = 'l'
        && String.unsafe_get s (!i+3) = 'l'
        then (i := !i + 4; J.atom i0 !i Null)
        else raise_notrace Exit
    | ' ' | '\t' | '\n' | '\r' -> incr i; value ()
    | c -> number c

  and number c =
    let i0 = !i in
    let c = if c = '-' then (incr i; get ()) else c in
    begin match c with
    | '0' -> incr i
    | '1'..'9' -> digits ()
    | _ -> raise_notrace Exit
    end;
    let as_float = ref false in
    if !i < n && String.unsafe_get s !i = '.' then begin
      incr i;
      as_float := true;
      if not (is_digit (get ())) then raise_notrace Exit;
      digits ()
    end;
    if !i < n && (match String.unsafe_get s !i with 'e'|'E' -> true | _ -> false) then begin
      incr i;
      as_float := true;
      let c = match get () with '+' | '-' -> incr i; get () | c -> c in
      if not (is_digit c) then raise_notrace Exit;
      digits ()
    end;
    try
      let s = String.sub s i0 (!i - i0) in
      if !as_float then J.atom i0 !i (float (float_of_string s))
      else
        match int_of_string s with
        | n -> J.atom i0 !i (int n)
        | exception _ ->
            (* Handle integer overflow.  Should we check that the float representation is faithful? *)
            J.atom i0 !i (float (float_of_string (s ^ ".")))
    with _ ->
      raise_notrace Exit (* the error message will be weird in that case... *)

  and obj i0 c acc =
    if c <> '"' then raise_notrace Exit;
    incr i;
    let label = string () in
    if next () <> ':' then raise_notrace Exit;
    incr i;
    let acc = (label, value ()) :: acc in
    match next () with
    | ',' -> incr i; obj i0 (next ()) acc
    | '}' -> incr i; J.object_ i0 !i (List.rev acc)
    | _ -> raise_notrace Exit

  and array i0 acc =
    let acc = value () :: acc in
    match next () with
    | ',' -> incr i; array i0 acc
    | ']' -> incr i; J.array i0 !i (List.rev acc)
    | _ -> raise_notrace Exit

  and string () =
    match get () with
    | '"' -> incr i; let str = Buffer.contents b in Buffer.clear b; str
    | '\\' ->
        incr i;
        let c = get () in
        incr i;
        begin match c with
        | '\"' -> Buffer.add_char b '\"'
        | '\\' -> Buffer.add_char b '\\'
        | '/' -> Buffer.add_char b '/'
        | 'b' -> Buffer.add_char b '\b'
        | 'f' -> Buffer.add_char b '\x0c'
        | 'n' -> Buffer.add_char b '\n'
        | 'r' -> Buffer.add_char b '\r'
        | 't' -> Buffer.add_char b '\t'
        | 'u' ->
            let cp = uchar () in
            if in_high_surrogate_range cp then begin
              if not (!i + 1 < n && String.unsafe_get s !i = '\\' && String.unsafe_get s (!i + 1) = 'u') then raise_notrace Exit;
              i := !i + 2;
              let cp2 = uchar () in
              if not (in_low_surrogate_range cp2) then raise_notrace Exit;
              let high_cp = (cp - 0xD800) lsl 10 in
              let low_cp = (cp2 - 0xDC00) in
              buffer_add_cp b (high_cp + low_cp + 0x1_0000)
            end else if in_low_surrogate_range cp then raise_notrace Exit
            else buffer_add_cp b cp
        | _ -> raise_notrace Exit
        end;
        string ()
    | '\032' .. '\127' | '\n' | '\r' | '\t' -> (* newline/tabs not officially supported! *)
        (* fast path for 7-bit ASCII *)
        let i0 = !i in
        incr i;
        while (match get () with '"' | '\\' -> false | '\032'..'\127' -> true | _ -> false) do incr i done;
        if String.unsafe_get s !i = '"' then
          let s0 = String.sub s i0 (!i - i0) in
          incr i;
          if Buffer.length b = 0 then s0 else let str = Buffer.contents b ^ s0 in Buffer.clear b; str
        else begin
          Buffer.add_substring b s i0 (!i - i0);
          string ()
        end
    | '\000' .. '\031' -> raise_notrace Exit
    | _ ->
        let d = String.get_utf_8_uchar s !i in
        if not (Uchar.utf_decode_is_valid d) then raise_notrace Exit;
        Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d);
        i := !i + Uchar.utf_decode_length d;
        string ()

  and uchar () =
    if !i + 4 > n then raise_notrace Exit;
    i := !i + 4;
    (hex (String.unsafe_get s (!i-4)) lsl 12)
    lor (hex (String.unsafe_get s (!i-3)) lsl 8)
    lor (hex (String.unsafe_get s (!i-2)) lsl 4)
    lor hex (String.unsafe_get s (!i-1))

  in
  try
    let v = value () in
    if check_eof && try ignore (next ()); true with Exit -> false then
      raise_notrace Exit;
    v, !i
  with Exit ->
    let (line, col) = pos s !i in
    let filename =
      match filename with
      | Some fn -> Printf.sprintf "file \"%s\", " (Filename.basename fn)
      | None -> ""
    in
    json_failuref "JSON parsing error at %sline %i, character %i: unexpected %s" filename line col
      (if !i = n then "end of input" else String.make 1 s.[!i])

let decode_internal ?filename s =
  fst (json_parser (module JsonBuilder_noloc) ~check_eof:true ?filename s 0)

let decode ?filename s =
  protect (fun () -> decode_internal ?filename s)

let deserialize ?filename ?(ctx=empty_ctx) ~t s =
  Result.bind (decode ?filename s) (of_json ~ctx ~t)

let decode_many_internal ?filename s =
  let rec loop accu i =
    (* drop initial whitespace? *)
    if i = String.length s then List.rev accu
    else
      let v, i = json_parser (module JsonBuilder_noloc) ~check_eof:false ?filename s i in
      loop (v :: accu) i
  in
  loop [] 0

let decode_many ?filename s =
  protect (fun () -> decode_many_internal ?filename s)

module Annotated = struct
  type 'a desc =
    | Atom of value
    | Array of 'a list
    | Object of (string * 'a) list
end

let decode_with_loc_internal (type t) ?filename mk s =
  let module M = struct
    open Annotated
    type nonrec t = t
    let atom start stop (x : value) = mk ~start ~stop (Atom x)
    let array start stop x = mk ~start ~stop (Array x)
    let object_ start stop x = mk ~start ~stop (Object x)
  end
  in
  fst (json_parser (module M) ~check_eof:true ?filename s 0)

let decode_with_loc ?filename mk s =
  protect (fun () -> decode_with_loc_internal ?filename mk s)

type pretty_options =
  {
    compact: bool option;
  }

let default_pretty_options =
  {
    compact = None
  }

let pp ppf x =
  let open Format in
  let escape s = Printf.sprintf "\"%s\"" (json_escaping s) in
  let pp_sep ppf () = pp_print_char ppf ','; pp_print_cut ppf () in
  let rec go ppf = function
    | Null -> pp_print_string ppf "null"
    | Bool b -> pp_print_bool ppf b
    | Number x -> pp_print_string ppf (Number.to_string x)
    | String s -> pp_print_string ppf (escape s)
    | Array [] -> pp_print_string ppf "[]"
    | Array a ->
        pp_print_char ppf '[';
        pp_print_break ppf 0 2;
        pp_open_vbox ppf 0;
        pp_print_list ~pp_sep go ppf a;
        pp_close_box ppf ();
        pp_print_cut ppf ();
        pp_print_char ppf ']'
    | Object [] -> pp_print_string ppf "{}"
    | Object l ->
        let go ppf (s, v) =
          pp_open_vbox ppf 0;
          pp_print_string ppf (escape s);
          pp_print_char ppf ':';
          pp_print_char ppf ' ';
          go ppf v;
          pp_close_box ppf ()
        in
        pp_print_char ppf '{';
        pp_print_break ppf 0 2;
        pp_open_vbox ppf 0;
        pp_print_list ~pp_sep go ppf l;
        pp_close_box ppf ();
        pp_print_cut ppf ();
        pp_print_char ppf '}'
  in
  pp_open_vbox ppf 0;
  go ppf x;
  pp_close_box ppf ()

let to_pretty_string ?(options = default_pretty_options) x =
  match options.compact with
  | Some true -> encode x
  | _ -> Format.asprintf "%a" pp x

module Access = struct

  type step =
    | Nth of int
    | Key of string

  type typ =
    | TyArray
    | TyObject
    | TyString
    | TyBool
    | TyNumber
    | TyNull

  let string_of_typ = function
    | TyArray -> "array"
    | TyObject -> "object"
    | TyString -> "string"
    | TyBool -> "bool"
    | TyNumber -> "number"
    | TyNull -> "null"

  let typeof = function
    | Object _     -> TyObject
    | Bool _       -> TyBool
    | Number _     -> TyNumber
    | Array _      -> TyArray
    | Null         -> TyNull
    | String _     -> TyString

  type path = step list

  type error_kind =
    | Key_unbound of string
    | Msg of string
    | Nth_unbound of int
    | Type_error of typ * typ
    | Alt_error of error * error

  and error =
    path * error_kind

  let string_of_step = function
    | Nth n -> string_of_int n
    | Key s -> s

  let string_of_path path =
    String.concat "." (List.rev_map string_of_step path)

  let rec string_of_error_kind = function
    | Key_unbound s -> Printf.sprintf "unbound key: %S" s
    | Msg s -> s
    | Nth_unbound n -> Printf.sprintf "unbound index: %d" n
    | Type_error (expected, got) -> Printf.sprintf "expected: %s, got: %s" (string_of_typ expected) (string_of_typ got)
    | Alt_error (err1, err2) ->
        Printf.sprintf "alt error: (%s) / (%s)"
          (string_of_error err1) (string_of_error err2)

  and string_of_error = function
    | [], errk ->
        string_of_error_kind errk
    | (_ :: _ as path), errk ->
        string_of_path path ^ ": " ^ string_of_error_kind errk

  exception Error of error

  type 'a t =
    path -> value -> 'a

  let app f q path v =
    (f path v) (q path v)

  let bind q f path v =
    f (q path v) path v

  let const v _ _ =
    v

  let map f q path v =
    f (q path v)

  let fail msg path _ =
    raise (Error (path, Msg msg))

  let failf fmt =
    Printf.ksprintf fail fmt

  let try_ p path v =
    match p path v with
    | x -> Stdlib.Ok x
    | exception Error err -> Stdlib.Error err

  let alt p q path v =
    try
      p path v
    with
    | Error err1 ->
        try
          q path v
        with Error err2 ->
          raise (Error (path, Alt_error (err1, err2)))

  let pair q r path v =
    q path v, r path v

  let query q v =
    match q [] v with
    | x -> Ok x
    | exception (Error err) -> Stdlib.Error err

  let value _ v =
    v

  let typerr ty path v =
    raise (Error (path, Type_error (ty, typeof v)))

  let string path = function
    | String s -> s
    | v -> typerr TyString path v

  let int path = function
    | Number x -> Number.round_to_int x
    | v -> typerr TyNumber path v

  let float path = function
    | Number x -> Number.to_float x
    | v -> typerr TyNumber path v

  let percentage = map (fun x -> x /. 100.) float

  let or_null q path = function
    | Null -> None
    | v -> Some (q path v)

  let or_null_empty q path = function
    | Null | String "" -> None
    | v -> Some (q path v)

  let bool path = function
    | Bool b -> b
    | v -> typerr TyBool path v

  let list q path = function
    | Array l ->
        List.mapi (fun i v -> q (Nth i :: path) v) l
    | v -> typerr TyArray path v

  let builder l mk path v =
    let fl = List.map (fun x -> x path v) l in
    mk (fun b -> List.iter (fun f -> f b) fl)

  let fold_assoc f q init path = function
    | Object l ->
        List.fold_left (fun acc (s, v) -> f s (q (Key s :: path) v) acc) init l
    | v ->
        typerr TyObject path v

  let assoc q path = function
    | Object l ->
        List.map (fun (s, v) -> s, q (Key s :: path) v) l
    | v ->
        typerr TyObject path v

  let hd q path = function
    | Array (v :: _) ->
        q (Nth 0 :: path) v
    | Array [] ->
        raise (Error (path, Nth_unbound 0))
    | v ->
        typerr TyArray path v

  let tl q path = function
    | Array (_ :: l) ->
        List.mapi (fun i v -> q (Nth (i+1) :: path) v) l
    | Array [] ->
        raise (Error (path, Nth_unbound 1))
    | v ->
        typerr TyArray path v

  let last q path = function
    | Array l ->
        let rec loop i = function
          | [] -> raise (Error (path, Nth_unbound 0))
          | [x] -> q (Nth i :: path) x
          | _ :: (_ :: _ as l) -> loop (i + 1) l
        in
        loop 0 l
    | v ->
        typerr TyArray path v

  let member ?(equal = String.equal) ?default s q path = function
    | Object l ->
        begin match List.find_opt (fun (x, _) -> equal s x) l with
        | Some (_, v) ->
            q (Key s :: path) v
        | None ->
            begin match default with
            | Some x -> x
            | None -> raise (Error (path, Key_unbound s))
            end
        end
    | v -> typerr TyObject path v

  let member_opt s q path = function
    | Object l ->
        begin match List.assoc_opt s l with
        | None | Some Null ->
            None
        | Some v ->
            Some (q (Key s :: path) v)
        end
    | v -> typerr TyObject path v

  let member_try field d =
    map (fun v -> Result.value v ~default:None) (try_ (member_opt field d))

  let empty_none f p = function
    | String "" as v -> typerr TyString p v
    | v ->  f p v

  module Infix = struct
    let (let*) = bind
    let (let+) q f = map f q
    let (and+) = pair
    let (and*) = pair
    let ($) = map
  end
end

let number x = Number x
(*
let () =
  register_conversion
    ~t:[%t: Mlfi_timestamp.t]
    ~to_json: (fun t -> string (Mlfi_timestamp.to_string t))
    ~of_json:
      (function
        | String s ->
            begin match Mlfi_timestamp.of_string s with
            | Some t -> Ok t
            | None -> Error (Printf.sprintf "Bad format for timestamp: %S" s)
            end
        | _ -> Error "Bad type for timestamp") *)
