(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

module Mlfi_types = Mlfi_types_internal

open Asttypes
open Lambda
open Mlfi_types

let lapply ap_func ap_args =
  Lapply
    {
      ap_func;
      ap_args;
      ap_loc = Loc_unknown;
      ap_tailcall = Default_tailcall;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
    }

let transl_path p =
  let env = Env.initial_with_auto () in
  transl_value_path Loc_unknown env p

(* Unique keys *)

module ExportTbl = Hashtbl.Make(struct
    type t = stype
    let equal = Internal.equal ~ignore_props:false ~ignore_path:false
    let hash = Internal.hash0
  end)

let stypes_to_record = ref []
let export_tbl = ExportTbl.create 8
let nstypes_to_record = ref 0
let stypes_table_id = ref (lazy (Ident.create_local "stypes_table"))
let normalize = Internal.normalize ~ignore_props:false ~ignore_path:false

let clear_tbl () =
  stypes_to_record := [];
  nstypes_to_record := 0;
  ExportTbl.clear export_tbl

let unique_key ty =
  let ty = normalize ty in
  try ExportTbl.find export_tbl ty
  with Not_found ->
    let id = !nstypes_to_record in
    stypes_to_record := ty :: !stypes_to_record;
    ExportTbl.replace export_tbl ty id;
    incr nstypes_to_record;
    id

let reset () =
  clear_tbl ();
  stypes_table_id := lazy (Ident.create_local "stypes_table")

let extract_ty ty =
  Lprim(Pfield (unique_key ty, Pointer, Immutable), [Lvar (Lazy.force !stypes_table_id)], Loc_unknown)

let extract_cst = function
  | Lconst c -> c
  | _ -> raise Exit

let block tag l =
  try Lconst(Const_block(tag, List.map extract_cst l))
  with Exit -> Lprim(Pmakeblock(tag, Immutable, None), l, Loc_unknown)

let tuple l = block 0 l

let rec list f = function
  | [] -> Lconst(Lambda.const_int 0)
  | hd :: tl -> tuple [f hd; list f tl]

let option f = function
  | None -> Lconst(Lambda.const_int 0)
  | Some h -> tuple [ f h ]

let cstr (c : stype) =
  let c = Obj.repr c in
  if Obj.is_int c then
    fun l ->
      assert (l = []);
      Lconst (Lambda.const_int (Obj.obj c))
  else
    let tag = Obj.tag c in
    let arity = Obj.size c in
    fun l ->
      assert (List.length l = arity);
      block tag l

let str s = Lconst (Const_immstring s)
let bool b = Lconst (Const_int (if b then 1 else 0))

let dt_abstract = cstr (DT_abstract ("", []))
let dt_arrow = cstr (DT_arrow ("", DT_int, DT_int))
let dt_list = cstr (DT_list DT_int)
let dt_tuple = cstr (DT_tuple [])
let dt_array = cstr (DT_array DT_int)
let dt_option = cstr (DT_option DT_int)
let dt_int = cstr DT_int
let dt_float = cstr DT_float
let dt_string = cstr DT_string
let dt_date = cstr DT_date
let dt_object = cstr (DT_object [])
let dt_polyvariant = cstr (DT_polyvariant [])
let dt_prop = cstr (DT_prop ([], DT_int))

let path_mlfi_types =
  Path.Pident(Ident.create_persistent "Mlfi_types")

let path_substitute =
  Path.Pdot (Path.Pdot (path_mlfi_types, "Internal"), "substitute")

let ttype_id_name = "$$TYPE$$"
let is_ttype_id id = Ident.name id = ttype_id_name

let rec mk_subst subst = function
  | DT_int -> dt_int []
  | DT_float -> dt_float []
  | DT_string -> dt_string []
  | DT_date -> dt_date []
  | t when not (Internal.has_var t) -> extract_ty t
  | DT_var i -> List.nth subst i
  | DT_abstract (s, tl) -> dt_abstract [ str s; list (mk_subst subst) tl ]
  | DT_arrow (l, t1, t2) ->
      dt_arrow  [ str l; mk_subst subst t1; mk_subst subst t2 ]
  | DT_tuple tl -> dt_tuple [ list (mk_subst subst) tl ]
  | DT_list t -> dt_list [ mk_subst subst t ]
  | DT_array t -> dt_array [ mk_subst subst t ]
  | DT_option t -> dt_option [ mk_subst subst t ]
  | DT_object l ->
      let f (s, t) = tuple [ str s; mk_subst subst t ] in
      dt_object [ list f l ]
  | DT_polyvariant l ->
      let f (s, t, p) = tuple [ str s; option (mk_subst subst) t; bool p ] in
      dt_polyvariant [ list f l ]
  | DT_prop (props, t) ->
      let f (k, v) = tuple [ str k; str v ] in
      dt_prop [ list f props; mk_subst subst t ]

  | DT_node _ as t ->
      (* note: we could share the creation of the subst tuple for the (rare) case
         where several nodes require substitution. *)
      let id = Ident.create_local ttype_id_name in
      let e = lapply (transl_path path_substitute) [tuple subst; extract_ty t] in
      Llet (Strict, Pgenval, id, e, Lvar id)


let transl_typeof env num =
  let (t, subst) = Typedynamic.get_stype num in
  let subst =
    List.map (fun path -> Lambda.transl_value_path Loc_unknown env path) subst
  in
  mk_subst subst t

let rec const_of_data (x: Obj.t) =
  if Obj.is_int x then Lambda.const_int (Obj.magic x : int)
  else
    let tag = Obj.tag x in
    if tag = Obj.string_tag then
      Const_immstring (Obj.magic x : string)
    else if tag <= Obj.last_non_constant_constructor_tag then
      let l = ref [] in
      for i = Obj.size x - 1 downto 0 do
        l := const_of_data (Obj.field x i) :: !l
      done;
      Const_block(tag, !l)
    else
      Misc.fatal_errorf "%s tag=%i" __FUNCTION__ tag

let dump_stypes =
  Sys.getenv_opt "DUMP_STYPES"

let path_import_table =
  Path.Pdot (Path.Pdot (path_mlfi_types, "Textual"), "import_table")

let add_mlfi_specific_to_module lambda =
  begin match dump_stypes with
  | None -> ()
  | Some dir ->
      let ext = if !Clflags.native_code then ".n.stype" else ".b.stype" in
      let fname = Filename.concat dir (Env.get_current_unit_name () ^ ext) in
      Typedynamic.dump_stypes fname
  end;
  let lambda = match List.rev !stypes_to_record with
    | [] -> lambda
    | l ->
        (* note: the order of the list l can be different in bytecode and native code due
           do the way the toplevel structures are compiled. *)
        let txt, digests = Textual.export_with_digests (DT_tuple l) in
        let tbl =
          lapply (transl_path path_import_table)
            [Lconst (const_of_data (Obj.magic txt));
             Lconst (const_of_data (Obj.magic digests))]
        in
        Llet(Strict, Pgenval, Lazy.force !stypes_table_id, tbl, lambda)
  in
  clear_tbl ();
  (* We do not clear stype2key here for the toplevel. *)
  lambda

let transl_typath transl_exp ~scopes loc steps =
  let block tag l = Lprim (Pmakeblock (tag, Immutable, None), l, Debuginfo.Scoped_location.of_location ~scopes loc) in
  let cons hd tl = block 0 [hd; tl] in
  let str s = Lconst (Const_immstring s) in
  let int n = Lconst (Const_int n) in
  let rec transl = function
    | [] -> int 0 (*nil*)
    | s :: rest -> cons (transl_step s) (transl rest)
  and transl_step : Typedynamic.Typath.step -> lambda = function
    | Ttypath_field tp -> block 0(*Field*) [str (Longident.last tp.txt)]
    | Ttypath_constructor (tp, n) ->
        block 1(*Constructor*) [str (Longident.last tp.txt); int n]
    | Ttypath_tuple (i, _) -> block 2(*Tuple_nth*) [int i]
    | Ttypath_list nth -> block 3(*List_nth*) [transl_exp ~scopes nth]
    | Ttypath_array nth -> block 4(*Array_nth*) [transl_exp ~scopes nth]
  in
  transl steps

let transl_exp transl_exp ~scopes e =
  match Typedynamic.Typath.decode e with
  | Some steps -> Some (transl_typath transl_exp ~scopes e.Typedtree.exp_loc steps)
  | None ->
      begin match Typedynamic.decode_typeof e with
      | Some (env, num) -> Some (transl_typeof env num)
      | None -> None
      end

let lift_ttype_exprs l =
  let bindings = ref [] in
  let fvs = ref Ident.Set.empty in
  let rec flush has_id body =
    let body = simplif body in
    if !bindings != [] && has_id !fvs then
      let tbl = Hashtbl.create 4 in
      let rec check acc = function
        | [] -> bindings := List.rev acc; body
        | (v, def, fv) :: rest when has_id fv ->
            begin match Hashtbl.find_opt tbl def with
            | Some v0 -> Llet (Alias, Pgenval, v, Lvar v0, check acc rest)
            | None -> Hashtbl.add tbl def v; Llet (Strict, Pgenval, v, def, check acc rest)
            end
        | b :: rest -> check (b :: acc) rest
      in
      check [] !bindings
    else
      body
  and simplif = function
    | Llet (_, _, id, def, body) when is_ttype_id id ->
        let fv = Lambda.free_variables def in
        fvs := Ident.Set.union !fvs fv;
        bindings := (id, def, fv) :: !bindings;
        simplif body
    | Lfunction f ->
        let r = flush (fun fv -> List.exists (fun (id, _) -> Ident.Set.mem id fv) f.params) in
        Lfunction (Lambda.map_lfunction r f)
    | Llet (str, kind, id, def, body) ->
        let def = simplif def in
        let body = flush (fun fv -> Ident.Set.mem id fv) body in
        Llet (str, kind, id, def, body)
    | lam ->
        Lambda.shallow_map simplif lam
  in
  let r = flush (fun _ -> true) l in
  assert(!bindings = []);
  r
