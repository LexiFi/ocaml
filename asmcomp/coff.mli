(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

(* This module implements a reader/writer for COFF object files
   and libraries. *)

type symbol
type section
type coff

module Symbol : sig
  val extern: string -> symbol
  val export: string -> section -> int32 -> symbol
  val named_intern: string -> section -> int32 -> symbol
end

module Reloc : sig
  val abs: [< `x64 | `x86 ] -> section -> int32 -> symbol -> unit
  val rel32: [< `x64 | `x86 ] -> section -> int32 -> symbol -> unit
end

module Section : sig
  val create: string -> int32 -> section
  val set_text: section -> string -> unit
end

module Coff : sig
  val create: [< `x64 | `x86 ] -> coff
  val add_section: coff -> section -> unit
  val add_symbol: coff -> symbol -> unit
  val put: out_channel -> coff -> unit
  val filter_symbols: coff -> (string -> bool) -> unit
end
