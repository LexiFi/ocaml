(***************************************************************************)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


module Internal =
struct
  (* keep in sync with translcore.ml. In particular, we currently assume Constructor has tag 1. *)
  type step =
    | Field of string
    | Constructor of string * int
    | Tuple_nth of int
    | List_nth of int
    | Array_nth of int

  let print_step ppf = function
    | Field s -> Format.fprintf ppf ".%s" s
    | Constructor (s, _) -> Format.fprintf ppf ".%s" s
    | Tuple_nth n -> Format.fprintf ppf ".(%i)" n
    | List_nth n -> Format.fprintf ppf ".[%i]" n
    | Array_nth n -> Format.fprintf ppf ".[|%i|]" n

  let print_steps ppf l =
    List.iter (print_step ppf) l
end

open Internal

let copy_patch_nth x n f =
  let x = Obj.dup x in
  Obj.set_field x n (f (Obj.field x n));
  x

type kind = [`Root|`Constructor|`Field|`List|`Array|`Tuple]

type ('a, 'b, +'c) t = step list
type ('a, 'b) field = ('a, 'b, [`Field]) t
type ('a, 'b) constructor = ('a, 'b, [`Constructor]) t
type ('a, 'b) tuple = ('a, 'b, [`Tuple]) t
type ('a, 'b) composed = ('a, 'b, kind) t
type ('a, 'b) list_step = ('a, 'b, [`List]) t
type ('a, 'b) array_step = ('a, 'b, [`Array]) t
type ('a, 'b) root = ('a, 'b, [`Root]) t

let steps_of_path x = x

let ( ^^ ) = ( @ )
let root = []

let type_path_get_list_nth i =
  [List_nth i]

let is_list_prefix = function
  | List_nth i :: rest -> Some (i, rest)
  | _ -> None

let type_path_get_array_nth i =
  [Array_nth i]

let field_name = function
  | [Field s] -> s
  | _ -> assert false

let constructor_name = function
  | [Constructor(n, _)] -> n
  | _ -> assert false

let extract_field_info ~t p find =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_node{rec_descr = DT_record{record_fields; record_repr; _}; _} ->
      begin
        let field = field_name p in
        match find (fun (s, _, _) -> s = field) record_fields with
        | None -> assert false
        | Some x -> (x, record_repr)
      end
  | s -> Format.eprintf "%a@." Mlfi_types.print_stype s; assert false

let extract_field_pos ~t p = extract_field_info ~t p List.find_index

let unsafe_ttype (t: Mlfi_types.stype): _ ttype =
  Obj.magic t

let extract_field_info ~t p =
  let (name, properties, (t: Mlfi_types.stype)), _ = extract_field_info ~t p List.find_opt in
  name, properties, unsafe_ttype t

let extract_field ~(t: 'a ttype) (p: ('a, 'b) field): 'a -> 'b =
  let nth, repr = extract_field_pos ~t p in
  match repr with
  | Record_float ->
      fun x -> Obj.magic (Obj.double_field (Obj.repr x) nth)
  | Record_regular | Record_inline _ ->
      fun x -> Obj.obj (Obj.field (Obj.repr x) nth)
  | Record_unboxed ->
      Obj.magic

let set_field ~(t: 'a ttype) (p: ('a, 'b) field): 'a -> 'b -> 'a =
  let nth, repr = extract_field_pos ~t p in
  match repr with
  | Record_float ->
      fun x v ->
        let x = Obj.dup (Obj.repr x) in
        Obj.set_double_field x nth (Obj.magic v);
        Obj.obj x
  | Record_regular | Record_inline _ ->
      fun x v ->
        let x = Obj.dup (Obj.repr x) in
        Obj.set_field x nth (Obj.repr v);
        Obj.obj x
  | Record_unboxed ->
      fun _ v -> Obj.magic v

let extract_field_type ~t p: _ ttype =
  let _ , _, t = extract_field_info ~t p in
  t

let is_empty: ('a, 'b, 'c) t -> ('a, 'b) Mlfi_types.TypEq.t option = fun path ->
  match steps_of_path path with
  | [] -> Some (Obj.magic Mlfi_types.TypEq.refl)
  | _ -> None

let non_constant_constructor_info ~t name =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_node{Mlfi_types.rec_descr = DT_variant{variant_constrs; variant_repr}; _} ->
      let rec aux id = function
        | [] -> assert false
        | (_, _, Mlfi_types.C_tuple []) :: constructors -> aux id constructors
        | (constructor_name, props, parameters) :: _ when constructor_name = name -> id, parameters, props, variant_repr
        | _ :: constructors -> aux (id + 1) constructors
      in
      aux 0 variant_constrs
  | DT_option t -> assert(name = "Some"); 0, C_tuple [t], [], Variant_regular
  | _ -> assert false

let constant_constructor_info ~t name =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_node{rec_descr = DT_variant{variant_constrs; variant_repr=Variant_regular}; _} ->
      let rec aux id = function
        | [] -> assert false
        | (constructor_name, props, Mlfi_types.C_tuple []) :: _ when constructor_name = name -> id, props
        | (_, _, C_tuple []) :: constructors -> aux (id + 1) constructors
        | _ :: constructors -> aux id constructors
      in
      aux 0 variant_constrs
  | DT_option _ -> assert(name = "None"); 0, []
  | _ -> assert false

let apply_constructor ~t = function
  | [ Constructor (name, 0) ] ->
      let tag, _ = constant_constructor_info ~t name in
      let x = Obj.magic tag in
      (fun _ -> x)
  | [ Constructor (name, 1) ] ->
      let tag, types, _, repr = non_constant_constructor_info ~t name in
      begin match repr with
      | Variant_unboxed ->
          Obj.magic
      | Variant_regular ->
          begin match types with
          | C_tuple [_] ->
              (fun x ->
                 let o = Obj.new_block tag 1 in
                 Obj.set_field o 0 (Obj.repr x);
                 Obj.magic o
              )
          | C_tuple ([] | _ :: _ :: _) -> assert false
          | C_inline _ ->
              (fun x ->
                 assert (Obj.tag (Obj.repr x) = tag);
                 Obj.magic x
              )
          end
      end
  | [ Constructor (name, n) ] ->
      assert (n > 1);
      let tag, _, _, _ = non_constant_constructor_info ~t name in
      (fun x ->  Obj.magic (Obj.with_tag tag (Obj.repr x)))
  | _ -> assert false

let tuple_info ~t n =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_tuple l -> List.nth l n
  | _ -> assert false

let extract_tuple_type ~t n =
  let (t: Mlfi_types.stype) = tuple_info ~t n  in
  unsafe_ttype t

let list_array_info ~t =
  match Mlfi_types.remove_first_props (Mlfi_types.stype_of_ttype t) with
  | DT_list t
  | DT_array t -> t
  | _ -> assert false

let rec patch ~t (f : Obj.t -> Obj.t) path (x : Obj.t) =
  match path with
  | [] -> f x
  | (List_nth _ | Array_nth _) :: _ ->
      invalid_arg __FUNCTION__
  (* Format.printf "%a\n" Internal.print_steps x; *)
  | Constructor (name, 0) :: rest ->
      assert (rest = []); (* A constant constructor points to a fake unit value by convention. No further steps are possible. *)
      if Obj.is_int x && (fst (constant_constructor_info ~t name) = Obj.magic x) then ignore (f (Obj.repr ()));
      x

  | Field _ as p :: rest ->
      let nth, repr = extract_field_pos ~t [p] in
      let t = extract_field_type ~t [p] in
      let v =
        let x =
          match repr with
          | Record_float -> Obj.repr (Obj.double_field x nth)
          | Record_regular | Record_inline _ -> Obj.field x nth
          | Record_unboxed -> x
        in
        patch ~t f rest x
      in
      begin match repr with
      | Record_float -> let x = Obj.dup x in Obj.set_double_field x nth (Obj.obj v); x
      | Record_regular | Record_inline _ -> let x = Obj.dup x in Obj.set_field x nth v; x
      | Record_unboxed -> v
      end
  | Tuple_nth n :: rest ->
      let t = extract_tuple_type ~t n in
      copy_patch_nth x n (patch ~t f rest)
  | Constructor (name, 1) :: rest ->
      let tag, types, _, repr = non_constant_constructor_info ~t name in
      begin match repr with
      | Variant_unboxed ->
          let typ = match types with C_inline typ | C_tuple [typ] -> typ | _ -> assert false in
          let t = unsafe_ttype typ in
          patch ~t f rest x
      | Variant_regular ->
          if tag <> Obj.tag (* returns Obj.int_tag for unboxed values, here constant constructors. *) x then x
          else begin
            match types with
            | C_tuple [typ] ->
                let t = unsafe_ttype typ in
                copy_patch_nth x 0 (patch ~t f rest)
            | C_tuple ([] | _ :: _ :: _) -> assert false
            | C_inline typ ->
                let t = unsafe_ttype typ in
                patch ~t f rest x
          end
      end
  | Constructor (name, n) :: rest ->
      assert (n > 1);
      let tag, types, _, _ = non_constant_constructor_info ~t name in
      if tag <> Obj.tag (* returns Obj.int_tag for unboxed values, here constant constructors. *) x then x
      else
        let t = unsafe_ttype (Mlfi_types.DT_tuple (Mlfi_types.uninline types)) in (* safe uninline since n > 1 *)
        x
        |> Obj.with_tag 0
        |> patch ~t f rest
        |> Obj.with_tag tag

let patch (type s) ~(t :s ttype) (path : (s, 'b, _) t) (x : s) (f : 'b -> 'b) : s =
  Obj.obj (patch ~t (Obj.magic f) path (Obj.repr x))


let extract_step (type t) ~(t: t ttype) step x =
  match step with
  | Field _ -> Mlfi_types.stype_of_ttype (extract_field_type ~t [step]), extract_field ~t [step] (Obj.obj x), []
  | Constructor(_, 0) -> Mlfi_types.stype_of_ttype [%t: unit], Obj.repr (), []
  | Constructor(name, _) ->
      let id, parameters, props, repr = non_constant_constructor_info ~t name in
      begin match repr with
      | Variant_unboxed ->
          Obj.magic t, x, props
      | Variant_regular ->
          if id <> Obj.tag x then failwith (Printf.sprintf "Extract step: bad constructor (not %S %i %i)" name id (Obj.tag x));
          begin match parameters with
          | C_tuple [t] -> t, Obj.field x 0, props
          | C_inline t -> t, x, props
          | C_tuple parameters -> DT_tuple parameters, Obj.with_tag 0 x, props
          end
      end
  | Tuple_nth n ->
      let t = tuple_info ~t n in
      t, Obj.field x n, []
  | List_nth n ->
      let rec aux n cons =
        if Obj.is_block cons then
          if n = 0 then
            Obj.field cons 0
          else
            let next_obj = Obj.field cons 1 in
            aux (n - 1) next_obj
        else
          failwith "Extract step: not enough elements in list"
      in
      let t = list_array_info ~t in
      t, aux n x, []
  | Array_nth n ->
      let length = Obj.size x in
      if length <= n then failwith "Extract step: not enough elements in array";
      let t = list_array_info ~t in
      t, Obj.field x n, []

let rec extract ~t path x =
  match path with
  | [] -> Obj.magic t, Obj.magic x
  | step :: path ->
      let t, x, _ = extract_step ~t step (Obj.repr x) in
      let t = unsafe_ttype t in
      let x = Obj.obj x in
      extract ~t path x

let extract_type_step (type t) ~(t: t ttype) = function
  | Field _ as step -> Mlfi_types.stype_of_ttype (extract_field_type ~t [step]), []
  | Constructor(name, 0) ->
      let _, props = constant_constructor_info ~t name in
      Mlfi_types.stype_of_ttype [%t: unit], props
  | Constructor(name, _) ->
      let _, parameters, props, _ = non_constant_constructor_info ~t name in
      begin match Mlfi_types.uninline parameters with
      | [t] -> t, props
      | l -> DT_tuple l, props
      end
  | Tuple_nth n ->
      tuple_info ~t n, []
  | List_nth _ ->
      list_array_info ~t, []
  | Array_nth _ ->
      list_array_info ~t, []

let rec extract_type ~t props = function
  | [] -> unsafe_ttype (Mlfi_types.stype_of_ttype t), props
  | step :: path ->
      let t, props = extract_type_step ~t step in
      let t = unsafe_ttype t in
      extract_type ~t props path

let extract_type ~t path = extract_type ~t [] path
let constructor_info ~t path =
  match path with
  | [Constructor(name, _)] ->
      let t, props = extract_type ~t path in
      name, props, t
  | _ -> assert false

let extract_type ~t path = fst(extract_type ~t path)

let has_shape ~t path x =
  try
    ignore(extract ~t path x);
    true
  with
  | Failure _ -> false

let force_shape (type s) ~(t :s ttype) (path : (s, 'b, _) t) (x : s) : s option =
  match List.rev path with
  | [] ->
      None
  | hd :: rev_path ->
      let path = List.rev rev_path in
      let t1 = extract_type ~t path in
      let doit f =
        let r = ref false in
        let x = patch ~t path x (fun _ -> r:= true; f) in
        if !r then Some x else None
      in
      match hd with
      | Constructor (name, 1) ->
          let tag, types, _, repr = non_constant_constructor_info ~t:t1 name in
          begin match types with
          | C_tuple [typ] when Mlfi_types.types_equality_modulo_props typ (Mlfi_types.stype_of_ttype [%t: unit]) ->
              let o =
                match repr with
                | Variant_unboxed -> Obj.repr ()
                | Variant_regular ->
                    let o = Obj.new_block tag 1 in
                    Obj.set_field o 0 (Obj.repr ());
                    o
              in
              doit o
          | _ -> None
          end
      | Constructor (name, 0) ->
          let tag, _ = constant_constructor_info ~t:t1 name in
          doit (Obj.repr tag)
      | _ ->
          None

let rec is_prefix prefix p =
  match prefix, p with
  | [], _ -> Some p
  | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> is_prefix tl1 tl2
  | _ -> None

type 'a rooted_path = TypePath: ('a, 'b, 'c) t -> 'a rooted_path

let prefix_rooted_path prefix (TypePath path) =
  TypePath (prefix ^^ path)

let rooted_root  = TypePath []

let composed x = x
