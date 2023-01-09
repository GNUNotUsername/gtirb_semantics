open Ocaml_protoc_plugin.Runtime
open Bytes
open List

type rectified_block = {
  uuid      : bytes;
  contents  : bytes;
  opcodes   : bytes list;
  (* Whatever ASLi gives back goes here later*)
  size      : int;
  offset    : int;
}

type content_block = {
  block    : Gtirb_semantics.ByteInterval.Gtirb.Proto.Block.t;
  raw      : bytes;
}

let opcode_length = 4;;

let () = 

  (* This is ugly; make a recursive n-dimensional version later *)
  let map2 f l = map (map f) l              in
  let map3 f l = map (map (map f)) l        in
  let map4 f l = map (map (map (map f))) l  in

  let rblock sz id = {uuid = id; contents = empty; opcodes = []; size = sz; offset = 0} in
  
  (* Byte manipulation convenience functions *)
  let len = Bytes.length in
  let b_tl op n = Bytes.sub op n (len op - n) in
  let b_hd op n = Bytes.sub op 0 n            in

  (* Main *)
  print_endline "";

  (* Read bytes from the file, skip first 8 *) 
  let bytes = 
    let ic  = open_in Sys.argv.(1)              in 
    let len = in_channel_length ic              in
    let _   = really_input_string ic 8          in
    let res = really_input_string ic (len - 8)  in
    close_in ic; 
    res
  in

  (* Pull out interesting code bits *)
  let raw     = Runtime'.Reader.create bytes                      in
  let gtirb   = Gtirb_semantics.IR.Gtirb.Proto.IR.from_proto raw  in
  let modules = (
    match gtirb with
    | Ok ir   -> ir.modules
    | Error e -> failwith (Printf.sprintf "%s%s" "Could not reply request: " (Ocaml_protoc_plugin.Result.show_error e))
  ) in
  let all_sects = map (fun (m : Gtirb_semantics.Module.Gtirb.Proto.Module.t) -> m.sections) modules                     in
  let all_texts = map (filter (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.name = ".text")) all_sects  in
  let intervals = map2 (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.byte_intervals) all_texts          in (* 2D list of all byte intervals *)
  let ival_blks = map3 (fun (i : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t)
      -> map (fun b -> {block = b; raw = i.contents}) i.blocks) intervals in

  (* Resolve polymorphic block variants to isolate info we actually care about *)
  let rectify   = function
    | `Code (c : Gtirb_semantics.CodeBlock.Gtirb.Proto.CodeBlock.t) -> rblock c.size c.uuid
    | `Data (d : Gtirb_semantics.DataBlock.Gtirb.Proto.DataBlock.t) -> rblock d.size d.uuid (* Might just delete these *)
    | _ -> rblock 0 empty
  in
  let poly_blks = map4 (fun b -> {{(rectify b.block.value) with offset = b.block.offset} with contents = b.raw}) ival_blks  in
  
  (* Section up byte interval contents to their respective blocks and slice out individual opcodes *)
  let trimmed   = map4 (fun b -> {b with contents = Bytes.sub b.contents b.offset b.size}) poly_blks                              in
  let rec cut_ops contents =
    if len contents <= opcode_length then [contents]
    else ((b_hd contents opcode_length) :: cut_ops (b_tl contents opcode_length)) in
  let op_cuts   = map4 (fun b -> {b with opcodes = cut_ops b.contents}) trimmed   in

  (* Convert every opcode to big endianness *)
  let need_flip = map (fun (m : Gtirb_semantics.Module.Gtirb.Proto.Module.t)
      -> m.byte_order = Gtirb_semantics.Module.Gtirb.Proto.ByteOrder.LittleEndian) modules in
  let rec endian_reverse opcode = 
    if len opcode = 1 then opcode
    else cat (endian_reverse (b_tl opcode 1)) (b_hd opcode 1)                       in
  let flip_opcodes block = {block with opcodes = map endian_reverse block.opcodes}  in
  (* Bless me father for I have sinned *)
  let rec fix_endianness flips blocks =
    (
      match flips with
      | []      -> []
      | h :: _  -> (if h then map3 flip_opcodes (hd blocks) else (hd blocks))
    ) :: (
      match flips with
      | []      -> []
      | _ :: t  -> fix_endianness t (tl blocks)
    )
  in
  let end_fixed = fix_endianness need_flip op_cuts in


  (* Testing *)
  let rec pp_ops ops = 
  match ops with
  | [] -> print_endline ""
  | h::t -> (print_endline (Hexstring.encode h); pp_ops t); in
  let dump_ops b = pp_ops b.opcodes in

  iter (iter (iter (iter dump_ops))) end_fixed