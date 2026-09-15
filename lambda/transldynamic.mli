(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


val reset: unit -> unit

val add_mlfi_specific_to_module: Lambda.lambda -> Lambda.lambda

val transl_exp:
  (scopes:Debuginfo.Scoped_location.scopes -> Typedtree.expression -> Lambda.lambda) ->
  scopes:Debuginfo.Scoped_location.scopes ->
  Typedtree.expression -> Lambda.lambda option

val lift_ttype_exprs: Lambda.lambda -> Lambda.lambda
