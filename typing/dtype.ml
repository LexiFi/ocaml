(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

open Types
open Parsetree

let empty =
  Ast_helper.Typ.mk Ptyp_any

let is_t_attr = function
  | {Parsetree.attr_name = {Location.txt = "t"}} -> true
  | _ -> false

let is_empty (sty : Parsetree.core_type) =
  sty.ptyp_attributes = [] &&
  sty.ptyp_desc = empty.ptyp_desc

let core_type_of_payload = function
  | Parsetree.PTyp sty -> Some sty
  | _ -> None

let core_type_of_attribute (x : Parsetree.attribute) =
  match x.attr_name.txt with
  | "#props#" -> core_type_of_payload x.attr_payload
  | _ -> None

let core_type_of_attributes attrs =
  match List.find_map core_type_of_attribute attrs with
  | None -> empty
  | Some sty -> sty

let core_types_of_attributes tyl attrs =
  match List.find_map core_type_of_attribute attrs with
  | Some {ptyp_desc = Ptyp_tuple styl; _} -> List.map snd styl
  | None -> List.map (fun _ -> empty) tyl
  | Some _ -> Misc.fatal_error __FUNCTION__

(* Propagation of constant expressions *)

let val_approx vd =
  let rec loop = function
    | {attr_name = {txt="mlfi.value_approx"};
       attr_payload =
         PStr [{pstr_desc=Pstr_eval
                    ({pexp_desc =
                        Pexp_constant {pconst_desc = Pconst_string (s, _, _)}}, _)}]; _} :: _ ->
        Some s
    | _ :: tl -> loop tl
    | [] -> None
  in
  loop vd.val_attributes

let approx_attr s =
  let module A = Ast_helper in
  A.Attr.mk (Location.mknoloc "mlfi.value_approx")
    (PStr [A.Str.eval (A.Exp.constant (A.Const.string s))])

let rec approx_expr env e =
  match e.Parsetree.pexp_desc with
  | Pexp_constant {pconst_desc = Pconst_string (s, _, _)} -> Some s
  | Pexp_ident lid ->
      begin
        try
          let (_, desc) = Env.lookup_value ~loc:lid.loc lid.txt env in
          val_approx desc
        with Not_found -> None (* More explicit error message? *)
      end
  | Pexp_apply ({pexp_desc = Pexp_ident{txt=Longident.Lident "^"}},
                [(Nolabel, e1); (Nolabel, e2)]) ->
      begin match approx_expr env e1 with
      | Some s1 ->
          begin match approx_expr env e2 with
          | Some s2 -> Some (s1 ^ s2)
          | _ -> None
          end
      | _ -> None
      end
  | Pexp_sequence (_, e2) -> approx_expr env e2
  | _ -> None

let warn_payload nm loc =
  Location.prerr_warning loc (Warnings.Attribute_payload (nm, "Invalid payload"))

let really_approx env a =
  match a.attr_payload with
  | PStr[{pstr_desc=Pstr_eval (e, _)}] ->
      approx_expr env e
  | _ ->
      None

let add_approx_attr env val_attrs attrs =
  match List.find_opt (fun a -> a.attr_name.txt = "val") val_attrs with
  | None -> attrs
  | Some a ->
      Builtin_attributes.mark_used a.attr_name;
      begin match really_approx env a with
      | None -> warn_payload "val" a.attr_loc; attrs
      | Some s -> approx_attr s :: attrs
      end

let props_attributes env (attrs : attributes) =
  let warn_payload loc = warn_payload "t" loc in
  let really_approx_expr e =
    match approx_expr env e with
    | None -> warn_payload e.pexp_loc; None
    | Some s ->
        Some {e with pexp_desc=Pexp_constant{pconst_desc=Pconst_string(s, Location.none, None); pconst_loc=e.pexp_loc}}
  in
  List.filter_map
    (fun a ->
       if a.attr_name.txt <> "t" then Some a
       else begin
         Builtin_attributes.mark_used a.attr_name;
         match a.attr_payload with
         | PStr[] -> None
         | PStr[{pstr_desc=Pstr_eval(e, [])} as p] ->
             let open Longident in
             let rec loop e =
               match e.pexp_desc with
               | Pexp_ident{txt=Lident _} ->
                   e
               | Pexp_apply({pexp_desc=Pexp_ident({txt=Lident "="})} as eq,
                            [Nolabel, ({pexp_desc=Pexp_ident{txt=Lident _}} as e1); Nolabel, e2]) ->
                   begin match really_approx_expr e2 with
                   | None -> raise Exit
                   | Some e2 -> {e with pexp_desc=Pexp_apply(eq, [Nolabel, e1; Nolabel, e2])}
                   end
               | Pexp_sequence(e1,e2) ->
                   {e with pexp_desc=Pexp_sequence(loop e1, loop e2)}
               | _ ->
                   warn_payload e.pexp_loc;
                   raise Exit
             in
             begin match loop e with
             | e -> Some {a with attr_payload = PStr[{p with pstr_desc=Pstr_eval(loop e, [])}]}
             | exception Exit -> None
             end
         | _ ->
             warn_payload a.attr_loc;
             None
       end
    ) attrs

let no_lid =
  Location.mknoloc (Longident.Lident "")

let rec prune_core_type env sty =
  let open Parsetree in
  let prune = prune_core_type env in
  let attrs = List.filter is_t_attr (props_attributes env sty.ptyp_attributes) in
  let mk desc = Ast_helper.Typ.mk ~attrs desc in
  let empty = mk empty.ptyp_desc in
  match sty.ptyp_desc with
  | Ptyp_any | Ptyp_var _ | Ptyp_extension _ ->
      empty
  | Ptyp_open (_, sty) ->
      let sty = prune sty in
      if is_empty sty then empty
      else mk (Ptyp_open (no_lid, sty))
  | Ptyp_arrow (lab, sty1, sty2) ->
      let sty1 = prune sty1 in
      let sty2 = prune sty2 in
      if is_empty sty1 && is_empty sty2 then empty
      else mk (Ptyp_arrow (lab, sty1, sty2))
  | Ptyp_tuple styl ->
      let styl = List.map (fun (_, sty) -> prune sty) styl in
      if List.for_all is_empty styl then empty else mk (Ptyp_tuple (List.map (fun sty -> None, sty) styl))
  | Ptyp_constr (_, styl) ->
      let styl = List.map prune styl in
      if List.for_all is_empty styl then empty else mk (Ptyp_constr (no_lid, styl))
  | Ptyp_class (_, styl) ->
      let styl = List.map prune styl in
      if List.for_all is_empty styl then empty else mk (Ptyp_class (no_lid, styl))
  | Ptyp_alias (sty, _) ->
      let sty = prune sty in
      if is_empty sty then empty else mk (Ptyp_alias (sty, Location.mknoloc ""))
  | Ptyp_poly (_, sty) ->
      let sty = prune sty in
      if is_empty sty then empty else mk (Ptyp_poly ([], sty))
  | Ptyp_object (l, _) ->
      let l =
        let mk desc = {pof_desc = desc; pof_loc = Location.none; pof_attributes = []} in
        List.map (function
            | {pof_desc = Otag (lab, sty); _} ->
                mk (Otag ({txt = lab.txt; loc = Location.none}, prune sty))
            | {pof_desc = Oinherit sty; _} ->
                mk (Oinherit (prune sty))
          ) l
      in
      if List.for_all (fun {pof_desc = Otag (_, sty) | Oinherit sty} -> is_empty sty) l
      then empty
      else mk (Ptyp_object (l, Closed))
  | Ptyp_variant (l, _, _) ->
      let l =
        let mk desc = {prf_desc = desc; prf_loc = Location.none; prf_attributes = []} in
        List.map (function
            | {prf_desc = Rtag (lab, _, styl); _} ->
                mk (Rtag ({txt = lab.txt; loc = Location.none}, false, List.map prune styl))
            | {prf_desc = Rinherit sty; _} ->
                mk (Rinherit (prune sty))
          ) l
      in
      if
        List.for_all (function
            | {prf_desc = Rtag (_, _, styl); _} -> List.for_all is_empty styl
            | {prf_desc = Rinherit sty; _} -> is_empty sty
          ) l
      then empty
      else mk (Ptyp_variant (l, Closed, None))
  | Ptyp_package {ppt_constraints; _} ->
      let l = List.map (fun (_, sty) -> no_lid, prune sty) ppt_constraints in
      if List.for_all (fun (_, sty) -> is_empty sty) l then empty
      else mk (Ptyp_package {ppt_path = no_lid; ppt_constraints = l; ppt_loc = Location.none; ppt_attrs = []})
  | Ptyp_functor (label, name, package, sty) ->
      let package =
        { package with
          ppt_path = no_lid;
          ppt_constraints =
            List.map (fun (_, sty) -> no_lid, prune sty)
              package.ppt_constraints;
        }
      in
      let sty = prune sty in
      if List.for_all (fun (_, sty) -> is_empty sty) package.ppt_constraints
         && is_empty sty
      then empty
      else mk (Ptyp_functor (label, name, package, sty))

let store_props env sty attrs =
  let sty = prune_core_type env sty in
  if is_empty sty then attrs
  else Ast_helper.Attr.mk (Location.mknoloc "#props#") (PTyp sty) :: attrs

let store_props_tuple env styl attrs =
  store_props env (Ast_helper.Typ.tuple (List.map (fun sty -> None, sty) styl)) attrs

let ident_props =
  Ident.create_persistent "Props:"

let encode_props props =
  Marshal.to_string props []

let decode_props s =
  Marshal.from_string s 0

let path_of_props props =
  Path.Pdot (Pident ident_props, encode_props props)

let props_of_path = function
  | Path.Pdot (Pident id, s) when Ident.same id ident_props -> Some (decode_props s)
  | _ -> None

let path_is_props = function
  | Path.Pdot (Pident id, _) -> Ident.same id ident_props
  | _ -> false

let get_str_props attrs =
  List.map
    (List.map
       (function
         | (k, {pexp_desc=Pexp_constant {pconst_desc = Pconst_string (s, _, _)}}) -> (k, s)
         | _ -> assert false
       )
    )
    (Ast_helper.get_props attrs)

let rec restore_props (sty : Parsetree.core_type) (ty : type_expr) : type_expr =
  let mk desc = Btype.newty2 ~level:(get_level ty) desc in
  let ty =
    match sty.ptyp_desc, get_desc ty with
    | Ptyp_arrow (_, sty1, sty2), Tarrow (lab, ty1, ty2, comm) ->
        let ty1' = restore_props sty1 ty1 and ty2' = restore_props sty2 ty2 in
        if ty1' == ty1 && ty2' == ty2 then ty else mk (Tarrow (lab, ty1', ty2', comm))
    | Ptyp_tuple styl, Ttuple tyl ->
        let tyl' = List.map2 (fun (_, sty) (lbl, ty) -> lbl, restore_props sty ty) styl tyl in
        if List.for_all2 (fun (_, ty') (_, ty) -> ty' == ty) tyl' tyl then ty else mk (Ttuple tyl')
    | Ptyp_constr (_, styl), Tconstr (path, tyl, memo) ->
        let tyl' = List.map2 restore_props styl tyl in
        if List.for_all2 (==) tyl' tyl then ty else mk (Tconstr (path, tyl', memo))
    | Ptyp_poly (_, sty), _ ->
        restore_props sty ty
    | Ptyp_object (fields, _), Tobject (ty1, flag) ->
        let rec loop ty =
          match get_desc ty with
          | Tfield(s, k, ty1, ty2) ->
              let ty1' =
                match
                  List.find_map (fun (pof : Parsetree.object_field) ->
                      match pof with
                      | {pof_desc = Otag ({txt = s'}, sty1)} ->
                          if s' = s then Some (restore_props sty1 ty1) else None
                      | {pof_desc = Oinherit _} ->
                          None
                    ) fields
                with
                | None -> ty1
                | Some ty1' -> ty1'
              in
              let ty2' = loop ty2 in
              if ty1' == ty1 && ty2' == ty2 then ty else mk (Tfield(s, k, ty1', ty2'))
          | _ ->
              ty
        in
        let ty1' = loop ty1 in
        if ty1' == ty1 then ty else mk (Tobject (ty1', flag))
    | Ptyp_variant (fields, _, _), Tvariant trow ->
        let trow' =
          let tfields = row_fields trow in
          let tfields' =
            List.map (fun ((s, tfield) as f) ->
                match
                  List.find_map (fun (prf : Parsetree.row_field) ->
                      match prf with
                      | {prf_desc = Rtag ({txt = s'}, _, styl)} -> if s' = s then Some styl else None
                      | {prf_desc = Rinherit _} -> None
                    ) fields
                with
                | None -> f
                | Some styl ->
                    begin match styl, row_field_repr tfield with
                    | [sty], Rpresent (Some ty) ->
                        let ty' = restore_props sty ty in
                        if ty' == ty then f
                        else s, rf_present (Some (restore_props sty ty))
                    | styl, Reither (no_arg, tyl, matched) ->
                        let tyl' = List.map2 restore_props styl tyl in
                        if List.for_all2 (==) tyl' tyl then f
                        else s, rf_either ~no_arg tyl' ~matched
                    | _ ->
                        f
                    end
              ) tfields
          in
          if List.for_all2 (==) tfields' tfields then trow
          else
            create_row
              ~fields:tfields'
              ~more:(row_more trow)
              ~closed:(row_closed trow)
              ~fixed:(row_fixed trow)
              ~name:(row_name trow)
        in
        if trow' == trow then ty else mk (Tvariant trow')
    | Ptyp_package {ppt_constraints; _}, Tpackage {pack_path; pack_constraints} ->
        let pack_constraints' =
          List.map2
            (fun (_, sty) (lid, ty) -> lid, restore_props sty ty)
            ppt_constraints pack_constraints
        in
        if List.for_all2
             (fun (_, ty') (_, ty) -> ty' == ty)
             pack_constraints' pack_constraints
        then ty
        else mk (Tpackage {pack_path; pack_constraints = pack_constraints'})
    | _ ->
        ty
  in
  List.fold_left (fun ty props ->
      let rec insert ty =
        match get_desc ty with
        | Tpoly (ty1, tyl) -> mk (Tpoly (insert ty1, tyl))
        | _ -> mk (Tconstr (path_of_props props, [ty], ref Mnil))
      in
      if props = [] then ty else insert ty
    ) ty (get_str_props sty.ptyp_attributes)

(* let restore_props sty ty = *)
(*   let ty' = restore_props sty ty in *)
(*   if ty' != ty then begin *)
(*     Format.printf "=> %a@." (Printast.payload 0) (Parsetree.PTyp sty); *)
(*     Format.printf "=> %a@." !Btype.print_raw ty; *)
(*     Format.printf "<= %a@." !Btype.print_raw ty' *)
(*   end; *)
(*   ty' *)

let restore_props_tuple attrs tyl =
  let styl = core_types_of_attributes tyl attrs in
  let tyl' = List.map2 restore_props styl tyl in
  if List.for_all2 (==) tyl' tyl then tyl else tyl'

let restore_props attrs ty =
  let sty = core_type_of_attributes attrs in
  restore_props sty ty

let has_props sg =
  with_type_mark begin fun mark ->
    let default = Btype.type_iterators mark in
    let it_do_type_expr iter ty =
      Btype.mark_type mark ty;
      match get_desc ty with
      | Tconstr (p, [_], _) when path_is_props p -> raise Exit
      | _ -> default.it_type_expr iter ty
    in
    let iter = {default with it_do_type_expr} in
    match iter.Btype.it_signature iter sg with
    | () -> false
    | exception Exit -> true
  end

let check_signature_for_cmi sg cmi =
  if has_props sg then
    Misc.fatal_errorf "%s: type properties should not be stored in .cmi files"
      (Unit_info.Artifact.filename cmi)

let has_props sdecl =
  let exception Found of attribute in
  let default = Ast_iterator.default_iterator in
  let attribute iter a = if a.attr_name.txt = "t" then raise (Found a); default.attribute iter a in
  let iter = {default with attribute} in
  match iter.Ast_iterator.type_declaration iter sdecl with
  | () -> None
  | exception Found a -> Some a

type typath_step =
  | Typath_constructor of Longident.t Location.loc * core_type option
  | Typath_field of Longident.t Location.loc * core_type option
  | Typath_tuple of int * int
  | Typath_list of expression
  | Typath_array of expression

let decode_typath ~loc payload =
  let rec inner acc ty_constraint e =
    match e.pexp_desc with
    | Pexp_ident lid ->
        Typath_field (lid, ty_constraint) :: acc
    | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident"/"}},
                  [Nolabel, {pexp_desc=Pexp_constant{pconst_desc = Pconst_integer(a,None)}};
                   Nolabel, {pexp_desc=Pexp_constant{pconst_desc = Pconst_integer(b,None)}}]) ->
        Typath_tuple (int_of_string a, int_of_string b) :: acc
    | Pexp_construct (lid, None) ->
        Typath_constructor (lid, ty_constraint) :: acc
    | Pexp_construct ({txt=Lident"::"}, Some{pexp_desc=Pexp_tuple [None, e; None, {pexp_desc=Pexp_construct({txt=Lident"[]"}, None)}]}) ->
        Typath_list e :: acc
    | Pexp_array [e] ->
        Typath_array e :: acc
    | Pexp_constraint (e, ty_constraint) ->
        inner acc (Some ty_constraint) e
    | _ ->
        raise Syntaxerr.(Error (Other e.pexp_loc))
  and outer ty_constraint e =
    match e.pexp_desc with
    | Pexp_field (e, lid) ->
        Typath_field (lid, ty_constraint) :: outer None e
    | Pexp_apply ({pexp_desc=Pexp_ident{txt=Ldot({txt=Lident"String"}, {txt="get"})}}, [Nolabel, e1; Nolabel, e2]) ->
        Typath_list e2 :: outer None e1
    | Pexp_apply ({pexp_desc=Pexp_ident{txt=Ldot({txt=Lident"Array"}, {txt="get"})}}, [Nolabel, e1; Nolabel, e2]) ->
        inner (outer None e1) ty_constraint e2
    | Pexp_constraint (e, ty_constraint) ->
        outer (Some ty_constraint) e
    | _ ->
        inner [] ty_constraint e
  in
  match payload with
  | PStr [] -> []
  | PStr [{pstr_desc = Pstr_eval(e,_)}] -> List.rev (outer None e)
  | _ -> raise Syntaxerr.(Error (Other loc))
