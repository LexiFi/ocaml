(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

open Parsetree
open Types

val store_props: Env.t -> core_type -> attributes -> attributes
val store_props_tuple: Env.t -> core_type list -> attributes -> attributes
val path_of_props: (string * string) list -> Path.t
val props_of_path: Path.t -> (string * string) list option
val path_is_props: Path.t -> bool
val restore_props: attributes -> type_expr -> type_expr
val restore_props_tuple: attributes -> type_expr list -> type_expr list
val props_attributes: Env.t -> attributes -> attributes
val val_approx: value_description -> string option
val approx_attr: string -> Parsetree.attribute
val add_approx_attr: Env.t -> attributes -> attributes -> attributes
val approx_expr: Env.t -> expression -> string option
val check_signature_for_cmi: signature -> Unit_info.Artifact.t -> unit
val get_str_props: attributes -> (string * string) list list
val has_props: Parsetree.type_declaration -> attribute option

type typath_step =
  | Typath_constructor of Longident.t Location.loc * core_type option
  | Typath_field of Longident.t Location.loc * core_type option
  | Typath_tuple of int * int
  | Typath_list of expression
  | Typath_array of expression

val decode_typath: loc:Location.t -> payload -> typath_step list
