(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

module Mlfi_types = Mlfi_types_internal

open Types
open Mlfi_types
open Path
open Btype
open Asttypes

let ident_typeof = Ident.create_persistent "%typeof"
let path_typeof = Pident ident_typeof
let path_call_site = Pdot(Pdot(Pident(Ident.create_persistent "Mlfi_types"), "StackTrace"), "call_site")
let path_type_path = Pdot(Pident(Ident.create_persistent "Mlfi_type_path"), "t")
let type_type_path alpha beta gamma = newgenty (Tconstr(path_type_path, [alpha; beta; gamma], ref Mnil))
let path_ttype = Pdot(Pident(Ident.create_persistent "Mlfi_types"), "ttype")
let type_ttype t = newgenty (Tconstr(path_ttype, [t], ref Mnil))

let type_typeof =
  lazy begin
    let tvar = newgenvar () in
    newgenty(Tarrow(Nolabel, Predef.type_int, newgenty(Tarrow(Nolabel, tvar, type_ttype tvar, commu_ok)), commu_ok))
  end

let val_typeof =
  lazy
    { val_kind = Val_prim (Primitive.simple ~name:"%typeof" ~arity:2 ~alloc:false);
      val_type = Lazy.force type_typeof;
      val_loc = Location.none;
      val_attributes = [];
      val_uid = Uid.internal_not_actually_unique }

type auto_type =
  | Auto_ttype of type_expr
  | Auto_call_site
  | Auto_none

let classify_auto_type env ty =
  (* Format.eprintf "%a@." Printtyp.type_expr ty; *)
  let ty = Ctype.maybe_instance_poly ty in
  match get_desc ty with
  | Tconstr _ ->
      let ty = Ctype.expand_head env ty in
      begin match get_desc ty with
      | Tconstr(p, [t], _) when Path.same p path_ttype -> Auto_ttype t
      | Tconstr(p, [], _) when Path.same p path_call_site -> Auto_call_site
      | _ -> Auto_none
      end
  | _ ->
      Auto_none

let has_implicit env ty =
  not !Clflags.pure_caml &&
  match classify_auto_type env ty with
  | Auto_ttype _ | Auto_call_site -> true
  | Auto_none -> false

(* Compute a global name for module bindings and type declarations *)
let root_attr s =
  [Ast_helper.Attr.mk (Location.mknoloc "#root#")
     (Parsetree.PTyp (Ast_helper.Typ.var s))]

let assign_global_names unit =
  let open Ast_mapper in
  let rec mapper path =
    let super = default_mapper in
    let module_binding _m pmb =
      let m = mapper (path ^ "." ^ Option.value ~default:"*_*" pmb.Parsetree.pmb_name.txt) in
      let pmb = super.module_binding m pmb in
      {pmb with Parsetree.pmb_attributes = pmb.Parsetree.pmb_attributes @ root_attr path}
    in
    let type_declaration _m td =
      {td with Parsetree.ptype_attributes = td.Parsetree.ptype_attributes @ root_attr path}
    in
    let expr m e =
      match e.Parsetree.pexp_desc with
      | Pexp_struct_item
          ({pstr_desc = Pstr_module {pmb_name = {txt}; _}; _}, _expr) ->
          let m = mapper ("*LOCAL*." ^ Option.value ~default:"*_*" txt) in
          super.expr m e
      | _ ->
          super.expr m e
    in
    {super with module_binding; type_declaration; expr}
  in
  mapper unit

let assign_global_names ast =
  let map = assign_global_names (Env.get_current_unit_name ()) in
  map.Ast_mapper.structure map ast

let rec remove_global_names sg =
  let clean_attrs attrs =
    List.filter (function {Parsetree.attr_name = {txt = "#root#"; _}; _} -> false | _ -> true) attrs
  in
  let rec clean_module_type = function
    | Mty_signature sg -> Mty_signature (remove_global_names sg)
    | Mty_functor (param, mty) -> Mty_functor (param, clean_module_type mty)
    | Mty_ident _ | Mty_alias _ as mty -> mty
  in
  let clean_item = function
    | Sig_value _ | Sig_typext _ | Sig_class _ | Sig_class_type _ as item -> item
    | Sig_modtype (id, decl, visibility) ->
        let decl = {decl with mtd_type = Option.map clean_module_type decl.mtd_type} in
        Sig_modtype (id, decl, visibility)
    | Sig_type (id, decl, rec_flag, visibility) ->
        let decl = {decl with type_attributes = clean_attrs decl.type_attributes} in
        Sig_type (id, decl, rec_flag, visibility)
    | Sig_module (id, presence, decl, rec_flag, visibility) ->
        let decl =
          {decl with
           md_type = clean_module_type decl.md_type;
           md_attributes = clean_attrs decl.md_attributes}
        in
        Sig_module (id, presence, decl, rec_flag, visibility)
  in
  List.map clean_item sg

(* Naming of types for runtime representations *)
let builtin_type =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (_, id) -> Hashtbl.add tbl id ())
    Predef.builtin_idents;
  Hashtbl.mem tbl

let ident_stdlib = Ident.create_persistent "Stdlib"

let rec full_name_mod ~lax env path =
  let open Parsetree in
  let open Path in
  let path = Env.normalize_module_path (if lax then None else Some Location.none) env path in
  let path = Out_type.rewrite_double_underscore_paths env path in
  match path with
  | Pident id ->
      if Ident.persistent id then Ident.name id
      else begin try
          let md = Env.find_module path env in
          let root =
            List.fold_left
              (fun root -> function
                 | {attr_name = {txt="#localmodule#"; _}; _} -> "*LOCAL*"
                 | {attr_name = {txt="#funarg#"; _}; _} -> "*FUNARG*"
                 | {attr_name = {txt="#root#"; _};
                    attr_payload = PTyp {ptyp_desc=Ptyp_var s}; _} -> s
                 | _ -> root
              ) "*UNKNOWN*" md.md_attributes
          in
          root ^ "." ^ Ident.name id
        with Not_found ->
          Printf.sprintf "*?*.%s" (Path.name path)
      end
  | Pdot (Pident id, s) when Ident.same id ident_stdlib ->
      s
  | Pdot (p, s) ->
      full_name_mod ~lax env p ^ "." ^ s
  | Papply(m1, m2) ->
      Printf.sprintf "%s(%s)"
        (full_name_mod ~lax env m1)
        (full_name_mod ~lax env m2)
  | Pextra_ty (p, _) ->
      full_name_mod ~lax env p

let full_name_typ ~lax env path =
  let open Parsetree in
  let open Path in
  match path with
  | Pident id when builtin_type id ->
      Ident.name id
  | Pident id ->
      begin try
        let td = Env.find_type path env in
        let root =
          List.fold_left
            (fun root -> function
               | {attr_name = {txt="#root#"; _};
                  attr_payload = PTyp {ptyp_desc=Ptyp_var s}; _} -> s
               | {attr_name = {txt="#localtype#"; _}; _} -> "*LOCAL*"
               | _ -> root
            ) "*UNKNOWN*" td.type_attributes
        in
        root ^ "." ^ Ident.name id
      with Not_found ->
        Printf.sprintf "*?*.%s" (Path.name path)
      end
  | Pdot (p, s) ->
      full_name_mod ~lax env p ^ "." ^ s
  | Papply _ ->
      assert false
  | Pextra_ty (p, _) ->
      full_name_mod ~lax env p

type type_kind =
  | Abstract
  | Concrete

type alert =
  | Not_a_global_type of type_kind * string
  | Bad_witness_for_abstract_type of string

let alert_kind = function
  | Not_a_global_type (Concrete, _) -> "not_a_global_concrete_type"
  | Not_a_global_type (Abstract, _) -> "not_a_global_abstract_type"
  | Bad_witness_for_abstract_type _ -> "bad_witness_for_abstract_type"

let alert_message = function
  | Not_a_global_type (_, s)
  | Bad_witness_for_abstract_type s -> s

(* Avoid duplicated warnings *)
let warns = Local_store.s_table Hashtbl.create 8
let warning loc w =
  let k = (loc, w) in
  if not (Hashtbl.mem !warns k) then begin
    Location.alert ~kind:(alert_kind w) loc (alert_message w);
    Hashtbl.add !warns k ();
  end

type error =
  | Illegal_dyn_use
  | Illegal_dyn_type of string * type_expr

exception Error of Location.t * error
let error loc err = raise (Error (loc, err))
let errstr loc ty s = error loc (Illegal_dyn_type(s, ty))
let illegal_dyn_use loc = error loc Illegal_dyn_use

let path_date =
  Pdot (Pident (Ident.create_persistent "Mlfi_date"), "t")

let ident_mlfi_acontract =
  Ident.create_persistent "Mlfi_acontract"

let ident_mlfi_contract =
  Ident.create_persistent "Mlfi_contract"

let path_mlfi_acontract_contract =
  Pdot (Pident ident_mlfi_acontract, "contract")

let path_mlfi_contract_contract =
  Pdot (Pident ident_mlfi_contract, "contract")

let path_mlfi_contract_observable =
  Pdot (Pident ident_mlfi_contract, "observable")

let path_mlfi_acontract_obs =
  Pdot (Pident ident_mlfi_acontract, "obs")

let path_is_contract path =
  Path.same path path_mlfi_acontract_contract ||
  Path.same path path_mlfi_contract_contract

let path_is_observable path =
  Path.same path path_mlfi_contract_observable ||
  Path.same path path_mlfi_acontract_obs

(* If [~lax] is [true], allow returning dangling paths (pointing to modules
   without a corresponding .cmi). This is safe when all we want is the name of a
   type to put inside a [DT_abstract], and enables certain linking tricks that
   restrict the visible .cmi files. *)

let path_name ~lax ~warn kind loc env path =
  let s = full_name_typ ~lax env path in
  if warn && String.contains s '*' then warning loc (Not_a_global_type (kind, s));
  s

let no_dynamic_type =
  "no_dynamic_type", ""

let no_ttype_warning =
  "no_ttype_warning", ""

let extract_props ty =
  match get_desc ty with
  | Tconstr (path, [ty'], _) ->
      begin match Dtype.props_of_path path with
      | None -> [], ty
      | Some l -> l, ty'
      end
  | _ -> [], ty

let stype_of_type env loc ty =

  let memotbl = Hashtbl.create 16 in
  let nb_used_types = ref (-1) in
  let used_types = ref [] in

  let existing_type path =
    let i =
      match List.find_opt (fun (_, path2) -> Path.same path path2) !used_types with
      | None ->
          let i = incr nb_used_types; !nb_used_types in
          used_types := (i, path) :: !used_types;
          i
      | Some (i, _) ->
          i
    in
    DT_var i
  in

  let build_dt_prop props t =
    if List.mem no_dynamic_type props then
      error loc (Illegal_dyn_type("no_dynamic_type property", ty));
    match props with
    | [] -> t
    | _ -> DT_prop(props, t)
  in
  let rec dyn ~warn rec_types t =
    let props, t = extract_props t in
    let warn = warn && not (List.mem no_ttype_warning props) in
    if props = [] then dyn' ~warn rec_types t else build_dt_prop props (dyn ~warn rec_types t)
  and dyn' ~warn rec_types t =
    let (depth, rtypes) = rec_types in
    if depth > 100 then
      errstr loc ty "maximum depth exceeded, probably because of non-guarded recursion";
    let rec_types = (depth + 1, rtypes) in
    match get_desc t with
    | Tvar _ ->
        errstr loc ty "type variable"
    | Tpoly (t, []) -> dyn ~warn rec_types t
    | Tpoly _ -> errstr loc ty "poly"
    | Tunivar _ -> errstr loc ty "univar"
    | Tfunctor _ -> errstr loc ty "module-dependent arrow"
    | Tarrow (label, t1, t2, _) ->
        (* TODO: should we add a '?' prefix for Optional ? *)
        DT_arrow (label_name label, dyn ~warn rec_types t1, dyn ~warn rec_types t2)
    | Ttuple tys -> DT_tuple (List.map (fun (_, ty) -> dyn ~warn rec_types ty) tys)
    | Tvariant row ->
        let Row {fields; closed; _} = row_repr row in
        if not closed then errstr loc ty "open poly variant";
        let fields =
          List.fold_right (fun (label, field) acc ->
              match row_field_repr field with
              | Rpresent topt ->
                  (label, topt, true) :: acc
              | Rabsent ->
                  acc
              | Reither (_, [], _) ->
                  (label, None, false) :: acc
              | Reither (_, [ty], _) ->
                  (label, Some ty, false) :: acc
              | Reither (_, (_ :: _ :: _), _) ->
                  errstr loc ty "conjunctive poly variant"
            ) fields []
        in
        let fields = List.sort (fun (n, _, _) (n', _, _) -> Stdlib.compare n n') fields in
        DT_polyvariant (List.map (function
            | (s, None, p) -> s, None, p
            | (s, Some t, p) -> s, Some (dyn ~warn rec_types t), p
          ) fields)
    | Tobject (ty, _) ->
        let (fields, rest) = Ctype.flatten_fields ty in
        begin match get_desc rest with
        | Tnil -> ()
        | Tvar _ -> errstr loc ty "open object type"
        | _ -> assert false
        end;
        let fields =
          List.fold_right
            (fun (n, k, t) l ->
              let is_mono =
                match get_desc t with
                | Tpoly (_, []) -> true
                | Tpoly (_, _) -> false
                | _ -> assert false
              in
               match field_kind_repr k with
               | (Fprivate | Fpublic) when is_mono -> (n, t) :: l
               | _ -> l)
            fields [] in
        let fields =
          List.sort (fun (n, _) (n', _) -> Stdlib.compare n n') fields in
        DT_object (List.map (fun (s, t) -> (s, dyn ~warn rec_types t)) fields)
    | Tsubst _ -> assert false
    | Tpackage{pack_path; pack_constraints} ->
        let s = Path.name pack_path in
        let s =
          match pack_constraints with
          | [] -> s
          | _ ->
              Printf.sprintf "%s with types %s" s
                (String.concat " "
                   (List.map
                      (fun (lid, _) -> String.concat "." lid)
                      pack_constraints))
        in
        DT_abstract(s, List.map (fun (_, ty) -> dyn ~warn rec_types ty) pack_constraints)
    | Tfield(_, _, _, _) | Tnil | Tlink _ -> assert false
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list -> DT_list(dyn ~warn rec_types ty_arg)
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_option -> DT_option(dyn ~warn rec_types ty_arg)
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array -> DT_array(dyn ~warn rec_types ty_arg)
    | Tconstr(path, [], _) when Path.same path Predef.path_int -> DT_int
    | Tconstr(path, [], _) when Path.same path Predef.path_string -> DT_string
    | Tconstr(path, [], _) when Path.same path Predef.path_float -> DT_float
    | Tconstr(path, [], _) when Path.same path Predef.path_char -> DT_abstract ("char", [])
    | Tconstr(path, [], _) when Path.same path Predef.path_floatarray -> DT_abstract ("floatarray", [])
    | Tconstr(path, [], _) when Path.same path path_date -> DT_date
    | Tconstr(path, [], _) when path_is_contract path -> DT_abstract ("Mlfi_contract.contract", [])
    | Tconstr(path, [ty], _) when path_is_observable path -> DT_abstract ("Mlfi_contract.observable", [dyn ~warn rec_types ty])
    | Tconstr(path, tys, _) ->
        let decl =
          try Env.find_type path env
          with Not_found ->
            errstr loc ty ("cannot find definition for " ^ Path.name path)
        in

        let props = List.flatten (Dtype.get_str_props decl.type_attributes) in
        let warn = warn && not (List.mem no_ttype_warning props) in
        let props = List.filter (fun p -> p <> no_ttype_warning) props in

        let typexp attrs ty =
          keeping_props
            (fun () -> Ctype.apply env decl.type_params (Dtype.restore_props attrs ty) tys)
        in
        let typexp_tuple attrs tyl =
          keeping_props
            (fun () ->
               List.map (fun ty -> Ctype.apply env decl.type_params ty tys)
                 (Dtype.restore_props_tuple attrs tyl)
            )
        in

        let force_abstract =
          List.exists (fun {Parsetree.attr_name = {txt; _}; _} -> txt = "mlfi.abstract") decl.type_attributes
        in
        let abstract_dynamic =
          List.exists (fun {Parsetree.attr_name = {txt; _}; _} -> txt = "mlfi.abstract_dynamic") decl.type_attributes
        in
        let is_real_abstract = match decl.type_kind with Type_abstract _ | Type_external _ -> true | _ -> false in
        let decl =
          match decl.type_kind, force_abstract  with
          | _, true -> {decl with type_kind = Type_abstract Definition; type_manifest = None}
          | Type_open, _ -> {decl with type_kind = Type_abstract Definition} (* handle extensible sum types as abstract ones *)
          | _ -> decl
        in
        let type_name kind =
          match decl with
          | {type_manifest = Some body; type_attributes = attrs} when abstract_dynamic ->
              (* This is used e.g. for type Ib_stdlib.variant, defined as Mlfi_isdatypes.variant, with constructors
                 exported. *)
              begin match get_desc (typexp attrs body) with
              | Tconstr(path, _, _) -> path_name ~lax:true ~warn Abstract loc env path
              | _ ->
                  errstr loc ty ("dynamic-abstract type does not expand to path name: " ^ Path.name path)
              end
          | {type_kind = Type_external s; _} -> s
          | _ ->
              path_name ~lax:false ~warn kind loc env path
        in

        let try_st_rec set f =
          let args_key = List.map get_id tys in
          let key = (path, args_key) in
          try Hashtbl.find memotbl key
          with Not_found ->
            let (depth, rtypes) = rec_types in
            if List.length (List.filter ((=) path) rtypes) >= 10 then errstr loc ty "non-regular recursion";
            let node = Internal.create_node (type_name Concrete) (List.map (dyn ~warn rec_types) tys) in
            let t = DT_node node in
            let t = build_dt_prop props t in
            Hashtbl.replace memotbl key t;
            set node (f (dyn ~warn (depth, path :: rtypes)));
            t
        in
        match decl with
        | {type_kind = Type_abstract _ | Type_external _; type_manifest = None} ->
            begin try
              if not is_real_abstract then raise Not_found;
              let vpath, vd = Env.find_value_by_name ~use:true (Untypeast.lident_of_path path) env in
              let ttype t =
                let p, _ = Env.find_type_by_name ~use:true (Longident.parse "Mlfi_types.ttype") env in
                Ctype.newty (Tconstr (p, [t], ref Mnil))
              in
              let et =
                List.fold_right
                  (fun arg res ->
                     Ctype.newty (Tarrow (Nolabel, ttype arg, res, commu_ok))
                  )
                  tys (ttype t)
              in
              let et = Ctype.duplicate_type et in
              let ok =
                Ctype.is_moregeneral env et vd.val_type
              in

              if ok then begin
                (* Format.eprintf "Witness found for abstract type %s: %a@." type_name Location.print arg_exp_loc; *)
                if tys <> [] then raise Not_found; (* only non-parametrized type for now *)
                build_dt_prop props (existing_type vpath)
              end else begin
                warning loc
                  (Bad_witness_for_abstract_type (full_name_typ ~lax:false env path));
                raise Not_found;
              end
            with Not_found ->
              (* if (try ignore (String.index type_name '#'); false with Not_found -> true) then *)
              (* TODO: warning *)
              build_dt_prop props (DT_abstract (type_name Abstract, List.map (dyn ~warn rec_types) tys))
              (* else errstr "GADT existential variable" *) (* see #3480 *)
            end
        | {type_kind = Type_abstract _; type_manifest = Some body; type_attributes = attrs} when abstract_dynamic ->
            begin match get_desc (typexp attrs body) with
            | Tconstr(path, tys, _) ->
                let ttys = List.map (dyn ~warn rec_types) tys in
                build_dt_prop props (DT_abstract (path_name ~lax:true ~warn Abstract loc env path, ttys))
            | _ -> errstr loc ty ("dynamic-abstract type does not expand to path name: " ^ Path.name path)
            end
        | {type_kind = Type_abstract _; type_manifest = Some body; type_attributes = attrs} ->
            assert (not abstract_dynamic);
            build_dt_prop props (dyn ~warn rec_types (typexp attrs body))
        | {type_kind = Type_variant (constrs, repr)} ->
            try_st_rec Internal.set_node_variant begin fun dyn ->
              let nconst_tag = ref 0 in
              List.map
                (fun {Types.cd_id = c; cd_args; cd_res = rt; cd_attributes = attrs; cd_uid = uid} ->
                   let c = Ident.name c in
                   Env.mark_constructor_used Env.Positive uid;
                   if rt <> None then errstr loc ty "GADT not supported for dynamic types";
                   let ts =
                     match cd_args with
                     | Cstr_tuple [] -> C_tuple []
                     | Cstr_tuple tyl ->
                         incr nconst_tag;
                         C_tuple (List.map dyn (typexp_tuple attrs tyl))
                     | Cstr_record fields ->
                         let fields =
                           List.map
                             (fun {Types.ld_id=s; ld_type=ty; ld_attributes=attrs} ->
                                (Ident.name s, List.flatten (Dtype.get_str_props attrs), dyn (typexp attrs ty))
                             ) fields
                         in
                         let node = Internal.create_node (Printf.sprintf "%s.%s" (type_name Concrete) c) [] in
                         let repr = match repr with Variant_regular -> Record_inline !nconst_tag | Variant_unboxed -> Record_unboxed in
                         Internal.set_node_record node (fields, repr);
                         incr nconst_tag;
                         C_inline (DT_node node)
                   in
                   (c, List.flatten (Dtype.get_str_props attrs), ts)
                ) constrs,
              match repr with
              | Types.Variant_regular -> Variant_regular
              | Types.Variant_unboxed -> Variant_unboxed
            end
        | {type_kind = Type_record (fields, repr)} ->
            try_st_rec Internal.set_node_record begin fun dyn ->
              List.map
                (fun {Types.ld_id=s; ld_type=ty; ld_attributes=attrs} ->
                   (Ident.name s, List.flatten (Dtype.get_str_props attrs), dyn (typexp attrs ty))
                ) fields,
              match repr with
              | Types.Record_regular -> Record_regular
              | Types.Record_float -> Record_float
              | Types.Record_unboxed _ -> Record_unboxed
              | _ -> assert false
            end
        | {type_kind = _} ->
            assert false (* turned into Type_abstract above *)
  in
  let r = dyn ~warn:true (0, []) ty in
  r, List.rev_map snd !used_types

let stype_of_type env loc ty =
  if Config.merlin then
    try
      stype_of_type env loc ty
    with Error _ ->
      DT_var (-1), [] (* dummy *)
  else
    stype_of_type env loc ty

let stype_tbl = Local_store.s_table Hashtbl.create 7

let decode_typeof = function
  | {Typedtree.exp_desc =
       Texp_apply({exp_desc = Texp_ident(_, _, {val_kind = Val_prim {prim_name = "%typeof"}})},
                  [Nolabel, Arg {exp_desc = Texp_constant(Const_int num)};
                   Nolabel, Arg {exp_type = ty}]);
     exp_env = env;
     exp_loc = loc} ->
      Some (env, loc, ty, num)
  | _ ->
      None

let build_stypes =
  let iter =
    let expr iter e =
      match decode_typeof e with
      | Some (env, loc, ty, num) ->
          assert (not (Hashtbl.mem !stype_tbl num) || Config.merlin);
          Hashtbl.replace !stype_tbl num (loc, stype_of_type env loc ty)
      | None ->
          Tast_iterator.default_iterator.expr iter e
    in
    {Tast_iterator.default_iterator with expr}
  in
  iter.structure iter

let decode_typeof e =
  match decode_typeof e with
  | None -> None
  | Some (env, _loc, _ty, num) -> Some (env, num)

let get_stype num =
  match Hashtbl.find_opt !stype_tbl num with
  | Some (_, x) -> x
  | None -> Misc.fatal_errorf "stype witness not found (num=%d)" num

let dump_stypes fn =
  let rec mkdir_rec dir =
    if Sys.file_exists dir then ()
    else (mkdir_rec (Filename.dirname dir); Sys.mkdir dir 0o777)
  in
  let styl = Hashtbl.fold (fun _ (loc, (stype, _)) accu -> (loc, stype) :: accu) !stype_tbl [] in
  if styl <> [] then begin
    mkdir_rec (Filename.dirname fn);
    Out_channel.with_open_bin fn (fun oc ->
        let ppf = Format.formatter_of_out_channel oc in
        List.iter (fun (loc, stype) ->
            Format.fprintf ppf "%a: %a@." Location.print_loc loc Mlfi_types.print_stype stype
          ) styl
      )
  end

let stype_num = Local_store.s_ref (-1)

let rec copy_known_part ty =
  match get_desc ty with
  | Tconstr (p, tl, abbrev) ->
      newty2 ~level:(get_level ty) (Tconstr (p, List.map copy_known_part tl, abbrev))
  | Tarrow (l, t1, t2, c) ->
      newty2 ~level:(get_level ty) (Tarrow (l, copy_known_part t1, copy_known_part t2, c))
  | Ttuple tl ->
      newty2 ~level:(get_level ty) (Ttuple (List.map (fun (lbl, ty) -> lbl, copy_known_part ty) tl))
  | _ ->
      ty

let ttype_of env loc ty =
  let open Typedtree in
  let num = incr stype_num; !stype_num in
  let mk desc ty =
    { exp_desc = desc;
      exp_loc = loc;
      exp_type = Ctype.instance ty;
      exp_extra = [];
      exp_env = env;
      exp_attributes = [] }
  in
  let mkid s = Location.mknoloc (Longident.Lident s) in
  let false_cstr = Env.find_ident_constructor Predef.ident_false env in
  mk (Texp_apply
        (mk (Texp_ident(path_typeof, mkid (Ident.name ident_typeof), Lazy.force val_typeof)) (Lazy.force type_typeof),
         [Nolabel, Arg(mk (Texp_constant(Const_int num)) Predef.type_int);
          Nolabel, Arg(mk (Texp_assert(mk (Texp_construct(mkid "false", false_cstr, [])) Predef.type_bool, loc)) ty)]))
    (type_ttype ty)

let reset () =
  Hashtbl.reset !stype_tbl;
  Hashtbl.reset !warns;
  stype_num := -1

(* Error report *)

let report_error ppf = function
  | Illegal_dyn_use ->
      Format_doc.fprintf ppf
        "This primitive can only be applied to its argument"
  | Illegal_dyn_type(s, ty) ->
      Format_doc.fprintf ppf
        "The type@ %a@ cannot be a dynamic type (%s)" Printtyp.Doc.type_expr ty s

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )

module Typath = struct
  type step =
    | Ttypath_constructor of Longident.t Location.loc * int
    | Ttypath_field of Longident.t Location.loc
    | Ttypath_tuple of int * int
    | Ttypath_list of Typedtree.expression
    | Ttypath_array of Typedtree.expression

  let dummy_type =
    Types.create_expr (Tvar None) ~level:0 ~scope:0 ~id:0

  let dummy_value_description =
    { Types.val_type = dummy_type;
      val_kind = Val_reg;
      val_loc = Location.none;
      val_attributes = [];
      val_uid = Shape.Uid.internal_not_actually_unique }

  let dummy_constructor_description cstr_tag =
    { Data_types.cstr_name = "";
      cstr_res = dummy_type;
      cstr_existentials = [];
      cstr_args = [];
      cstr_arity = 0;
      cstr_tag;
      cstr_consts = 0;
      cstr_nonconsts = 0;
      cstr_generalized = false;
      cstr_private = Private;
      cstr_loc = Location.none;
      cstr_attributes = [];
      cstr_inlined = None;
      cstr_uid = Shape.Uid.internal_not_actually_unique }

  let dummy_path =
    Path.Pident (Ident.create_persistent "")

  let mkexp desc =
    { Typedtree.exp_desc = desc;
      exp_loc = Location.none;
      exp_extra = [];
      exp_type = dummy_type;
      exp_env = Env.empty;
      exp_attributes = [] }

  let mkident lid =
    mkexp (Texp_ident (dummy_path, lid, dummy_value_description))

  let mkint n =
    mkexp (Texp_constant (Const_int n))

  let mktuple l =
    mkexp (Texp_tuple l)

  let encode = function
    | Ttypath_constructor (lid, arity) ->
        mktuple [None, mkident lid; None, mkint arity]
    | Ttypath_field lid ->
        mkident lid
    | Ttypath_tuple (n, m) ->
        mktuple [None, mkint 0; None, mkint n; None, mkint m]
    | Ttypath_list e ->
        mktuple [None, mkint 1; None, e]
    | Ttypath_array e ->
        mktuple [None, mkint 2; None, e]

  let decode e =
    match e.Typedtree.exp_desc with
    | Texp_tuple [None, {exp_desc = Texp_ident (_, lid, _)}; None, {exp_desc = Texp_constant (Const_int n)}] ->
        Ttypath_constructor (lid, n)
    | Texp_ident (_, lid, _) ->
        Ttypath_field lid
    | Texp_tuple [None, {exp_desc = Texp_constant (Const_int 0)}; None, {exp_desc = Texp_constant (Const_int n)}; None, {exp_desc = Texp_constant (Const_int m)}] ->
        Ttypath_tuple (n, m)
    | Texp_tuple [None, {exp_desc = Texp_constant (Const_int 1)}; None, e] ->
        Ttypath_list e
    | Texp_tuple [None, {exp_desc = Texp_constant (Const_int 2)}; None, e] ->
        Ttypath_array e
    | _ ->
        Misc.fatal_error __FUNCTION__

  let encode steps =
    let cstr_tag =
      match steps with
      | [] -> Data_types.Cstr_constant 0 (* [] *)
      | _ :: _ -> Data_types.Cstr_block 0 (* (::) *)
    in
    Typedtree.Texp_construct (Location.mknoloc (Longident.Lident "#typath#"), dummy_constructor_description cstr_tag, List.map encode steps)

  let decode e =
    match e.Typedtree.exp_desc with
    | Texp_construct ({txt = Lident "#typath#"}, _, el) ->
        Some (List.map decode el)
    | _ ->
        None
end

let unshare_ttype (node : Typedtree.expression) =
  if not !Clflags.pure_caml then
    match get_desc node.exp_type with
    | Tconstr (p, [_t], _) when Path.same path_ttype p ->
        {node with exp_type = copy_known_part node.exp_type}
    | _ -> node
  else
    node
