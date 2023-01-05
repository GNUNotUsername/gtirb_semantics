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
  let all_sects = map (fun (m : Gtirb_semantics.Module.Gtirb.Proto.Module.t) -> m.sections) result                          in
  let all_texts = map (filter (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.name = ".text")) all_sects      in
  let intervals = map (map (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.byte_intervals)) all_texts         in (* 2D list of all byte intervals *)
  let ival_blks = map (map (map (fun (i : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t) -> i.blocks))) intervals in
  let get_offsets (block : Gtirb_semantics.ByteInterval.Gtirb.Proto.Block.t) = block.offset                                 in
  let bsizes    = map (map (map (map get_offsets))) ival_blks                                                               in
  iter (iter (iter (iter (Printf.printf "%d\n")))) bsizes