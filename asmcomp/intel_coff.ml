(***********************************************************************)
(*                                                                     *)
(*                              OCaml                                  *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)
(*
  Contributors:
  * Fabrice LE FESSANT (INRIA/OCamlPro)
*)

[@@@ocaml.warning "+A-42-4"]


open X86_ast
open X86_proc
open Intel_assembler
open! Coff

module String = Misc.Stdlib.String

let env_OCAMLASM =
  try
    String.split_on_char ':' (Sys.getenv "OCAMLASM")
  with Not_found -> []

let verbose = List.mem "verbose" env_OCAMLASM
let disable = List.mem "no" env_OCAMLASM
let force = List.mem "yes" env_OCAMLASM


let relocate_buffer b add_relative_reloc add_absolute_reloc =

  List.iter
    (fun (pos, k) ->
       match k with

       | RELOC_DIR32 (s, offset) ->
           add_absolute_reloc pos s;
           add_patch b pos B32 offset

       | RELOC_DIR64 (s, offset) ->
           add_absolute_reloc pos s;
           add_patch b pos B64 offset

       | RELOC_REL32 (s, offset) ->
           add_relative_reloc pos s;
           add_patch b pos B32 offset
    )
    (relocations b);

  ()

let machine_of_arch = function
    X64 -> `x64
  | X86 -> `x86

let create_coff arch text data : coff =
  let machine = machine_of_arch arch in
  let coff = Coff.create  machine in

  let align = match arch with
      X64 -> 0x0050_0000_l (* IMAGE_SCN_ALIGN_16BYTES *)
    | X86 -> 0x0030_0000_l (* IMAGE_SCN_ALIGN_4BYTES *)
  in
  let sects = [
    (* IMAGE_SCN_MEM_WRITE + IMAGE_SCN_MEM_READ +
       IMAGE_SCN_ALIGN_4BYTES + 401 *)
    (* Why 401 ? *)
    Section.create ".data"
      (Int32.logor align 0xC000_0040l), data;
    (* IMAGE_SCN_MEM_READ + IMAGE_SCN_MEM_EXECUTE +
       IMAGE_SCN_ALIGN_4BYTES + IMAGE_SCN_LNK_INFO + 01 *)
    (* Why 01 ? *)
    Section.create ".text"
      (Int32.logor align 0x6000_0020l), text;
  ]
  in
  let syms = Hashtbl.create 16 in
  let interns = Hashtbl.create 16 in
  let create_section (sect, b) =
    Coff.add_section coff sect;
    String.Map.iter
      (fun s sy ->
         match sy.sy_pos with
         | None -> ()
         | Some pos ->
             let sym =
               if sy.sy_global then begin
                 (* Printf.fprintf stderr "global %s\n%!" s; *)
                 Symbol.export s sect (Int32.of_int pos)
               end
               else begin
                 (* Printf.fprintf stderr "intern %s\n%!" s; *)
                 Hashtbl.replace interns s false;
                 Symbol.named_intern s sect (Int32.of_int pos)
               end
             in
             Hashtbl.replace syms s sym;
             Coff.add_symbol coff sym;
      ) (labels b)
  in
  List.iter create_section sects;
  List.iter
    (fun (sect, b) ->

       let add_local_symbol reloc pos s =
         (* Printf.fprintf stderr "add_local_symbol %s\n%!" s; *)
         if Hashtbl.mem interns s then
           Hashtbl.replace interns s true;

         let sym =
           try Hashtbl.find syms s
           with Not_found ->
             let sym = Symbol.extern s in
             Hashtbl.replace syms s sym;
             Coff.add_symbol coff sym;
             sym
         in
         reloc machine sect (Int32.of_int pos) sym
       in

       relocate_buffer b
         (add_local_symbol Reloc.rel32) (add_local_symbol Reloc.abs);

       Section.set_text sect (Intel_assembler.contents b)
    ) sects;

  Coff.filter_symbols coff (fun s -> try Hashtbl.find interns s with Not_found -> true);
  coff

let split_sections instrs =
  let sections = ref String.Map.empty in
  let section s =
    try
      String.Map.find s !sections
    with Not_found ->
      let section = (ref [], { sec_name = s; sec_instrs = [||] }) in
      sections := String.Map.add s section !sections;
      section
  in
  let current_section = ref (section ".text") in
  List.iter
    (function
       | Section ([sec], _, _) ->
           let sec =
             match sec with
             | ".rdata" | ".data" -> ".data"
             | ".text" -> ".text"
             | _ -> print_endline sec; assert false
           in
           current_section := section sec
       | ins ->
           let (section, _) = !current_section in
           section := ins :: !section
    )
    instrs;
  String.Map.map
    (fun (ref, section) ->
       { section with sec_instrs = Array.of_list (List.rev !ref) }
    )
    !sections


let assemble machine instrs outfile =
  if verbose then Printf.eprintf "[binary backend] assembling...\n";

  let sections = split_sections instrs in

  let text = String.Map.find ".text" sections in
  let data = String.Map.find ".data" sections in

  let data_buffer = assemble_section machine data in
  let text_buffer = assemble_section machine text in

  let coff = create_coff machine text_buffer data_buffer in
  if verbose then Printf.eprintf "[binary backend] done\n";

  let oc = open_out_bin outfile in
  Coff.put oc coff;
  close_out oc;
  if verbose then Printf.eprintf "[binary backend] object file created\n"

let backend_available =
  if disable then begin
    Printf.eprintf "[binary backend] disabled\n";
    false
  end else begin match X86_proc.system with
    | S_win64 | S_mingw64 ->
        if verbose then Printf.eprintf "[binary backend] installed\n";
        X86_proc.register_internal_assembler (assemble X64);
        true
    | _ ->
        if force || verbose then
          Printf.eprintf "[binary backend] not available!\n";
        if force then
          exit 2;
        false
  end
