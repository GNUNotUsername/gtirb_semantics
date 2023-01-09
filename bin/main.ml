open Ocaml_protoc_plugin.Runtime
open Bytes
open List

type rectified_block = {
  uuid      : bytes;
  contents  : bytes;
  (* Whatever ASLi gives back goes here later*)
  size      : int;
  offset    : int;
}

type content_block = {
  block    : Gtirb_semantics.ByteInterval.Gtirb.Proto.Block.t;
  raw      : bytes;
}

let () = 

  print_endline "";

  let death = "Could not reply request: "     in
  (* Read bytes from the file, skip first 8*) 
  let bytes = 
    let ic  = open_in Sys.argv.(1)              in 
    let len = in_channel_length ic              in
    let _   = really_input_string ic 8          in
    let res = really_input_string ic (len - 8)  in
    close_in ic; 
    res
  in

  (* TODO Make an N-dimensional map function *)
  let map2 f l = map (map f) l              in
  let map3 f l = map (map (map f)) l        in
  let map4 f l = map (map (map (map f))) l  in

  let rblock sz id = {uuid = id; contents = empty; size = sz; offset = 0} in

  let raw     = Runtime'.Reader.create bytes                      in
  let gtirb   = Gtirb_semantics.IR.Gtirb.Proto.IR.from_proto raw  in
  let result  = (
    match gtirb with
    | Ok ir   -> ir.modules
    | Error e -> failwith (Printf.sprintf "%s%s" death (Ocaml_protoc_plugin.Result.show_error e))
  ) in
  let all_sects = map (fun (m : Gtirb_semantics.Module.Gtirb.Proto.Module.t) -> m.sections) result                      in
  let all_texts = map (filter (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.name = ".text")) all_sects  in
  let intervals = map2 (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.byte_intervals) all_texts          in (* 2D list of all byte intervals *)
  (*let contents  = map3 (fun (i : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t) -> i.contents) intervals      in*)
  let ival_blks = map3 (fun (i : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t)
      -> map (fun b -> {block = b; raw = i.contents}) i.blocks) intervals in
  let rectify   = function
    | `Code (c : Gtirb_semantics.CodeBlock.Gtirb.Proto.CodeBlock.t) -> rblock c.size c.uuid
    | `Data (d : Gtirb_semantics.DataBlock.Gtirb.Proto.DataBlock.t) -> rblock d.size d.uuid
    | _ -> rblock 0 empty
  in
  (*let offsets   = map4 (fun (b : Gtirb_semantics.ByteInterval.Gtirb.Proto.Block.t) -> b.offset) ival_blks in*)
  let poly_blks = map4 (fun b -> {{(rectify b.block.value) with offset = b.block.offset} with contents = b.raw}) ival_blks  in
  let trimmed   = map4 (fun b -> {b with contents = sub b.contents b.offset b.size}) poly_blks                              in
  let visible   = map4 (fun b -> Hexstring.encode b.contents) trimmed                                                       in
  iter (iter (iter (iter print_endline))) visible