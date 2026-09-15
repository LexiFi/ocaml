(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

module Mlfi_types = Mlfi_types_internal

open Types

val type_type_path: type_expr -> type_expr -> type_expr -> type_expr

val assign_global_names: Parsetree.structure -> Parsetree.structure

val remove_global_names: Types.signature -> Types.signature

val full_name_mod: lax:bool -> Env.t -> Path.t -> string

val illegal_dyn_use: Location.t -> 'a

val stype_of_type: Env.t -> Location.t -> type_expr -> Mlfi_types.stype * Path.t list

val ttype_of: Env.t -> Location.t -> type_expr -> Typedtree.expression

val build_stypes: Typedtree.structure -> unit

val get_stype: int -> Mlfi_types.stype * Path.t list

val dump_stypes: string -> unit

val reset: unit -> unit

val decode_typeof: Typedtree.expression -> (Env.t * int) option

type auto_type =
  | Auto_ttype of type_expr
  | Auto_call_site
  | Auto_none

val classify_auto_type: Env.t -> type_expr -> auto_type
val has_implicit: Env.t -> type_expr -> bool

module Typath: sig
  type step =
    | Ttypath_constructor of Longident.t Location.loc * int
    | Ttypath_field of Longident.t Location.loc
    | Ttypath_tuple of int * int
    | Ttypath_list of Typedtree.expression
    | Ttypath_array of Typedtree.expression

  val encode: step list -> Typedtree.expression_desc
  val decode: Typedtree.expression -> step list option
end

val unshare_ttype: Typedtree.expression -> Typedtree.expression
