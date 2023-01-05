open Ocaml_protoc_plugin.Runtime
open List

let () = 

  (* Read bytes from the file, skip first 8*) 
  let bytes = 
    let ic  = open_in Sys.argv.(1)              in 
    let len = in_channel_length ic              in
    let _   = really_input_string ic 8          in
    let res = really_input_string ic (len - 8)  in
    close_in ic; 
    res
  in
  
  (* TODO make N dimensional map function later, this map (map (map )) business is silly
  This doesn't work
  let rec map_nd n f l =
  if n = 1 then map f l
  else map_nd (n - 1) (map f) l*)

  let raw     = Runtime'.Reader.create bytes                      in
  let gtirb   = Gtirb_semantics.IR.Gtirb.Proto.IR.from_proto raw  in
  let result  = (
    match gtirb with
    | Ok ir   -> ir.modules
    | Error e -> failwith (Printf.sprintf "Could not reply request: %s" (Ocaml_protoc_plugin.Result.show_error e))
  ) in
  let sections (modul : Gtirb_semantics.Module.Gtirb.Proto.Module.t) = modul.sections               in
  let all_sects = map sections result                                                               in
  let is_text (section : Gtirb_semantics.Section.Gtirb.Proto.Section.t) = section.name = ".text"    in
  let all_texts = map (filter is_text) all_sects                                                    in
  let sect_ivals (section : Gtirb_semantics.Section.Gtirb.Proto.Section.t) = section.byte_intervals in
  let intervals = map (map sect_ivals) all_texts                                                    in
  let int_conts (ival : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t) = ival.contents    in
  let raw_conts = map (map (map int_conts)) intervals                                               in
  let hex_conts = map (map (map Hexstring.encode)) raw_conts                                        in
  iter (iter (iter print_endline)) hex_conts