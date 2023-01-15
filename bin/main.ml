open Ocaml_protoc_plugin.Runtime
open Gtirb_semantics.IR.Gtirb.Proto
open Gtirb_semantics.ByteInterval.Gtirb.Proto
open Gtirb_semantics.Module.Gtirb.Proto
open Gtirb_semantics.Section.Gtirb.Proto
open Gtirb_semantics.CodeBlock.Gtirb.Proto
(*open Gtirb_semantics.DataBlock.Gtirb.Proto*)
open LibASL
open Bytes
open List

type rectified_block = {
  ruuid      : bytes;
  contents  : bytes;
  opcodes   : bytes list;
  address   : int;
  offset    : int;
  size      : int;
}

type ast_block = {
  auuid   : bytes;
  asts    : string list list;
  concat  : string;
}

(*
type serialisable = {
  suuid : bytes;
  sasts : bytes list list;
}*)

type content_block = {
  block   : Block.t;
  raw     : bytes;
  address : int; 
}

let opcode_length = 4
let binary_ind    = 1
let prelude_ind   = 2
let specs_start   = 3

let hex           = "0x"
let l_op          = "["
let l_dl          = ","
let l_cl          = "]"
let newline       = "\n"
let space         = " "
let strung        ="\""
(*
let ll_op         = "["
let ll_dl         = ","
let ll_cl         = "]"
let ast_op        = "{"
let ast_cl        = "}"*)

let () = 

  (* This is ugly; make a recursive n-dimensional version later *)
  let map2 f l = map (map f) l              in
  let map3 f l = map (map (map f)) l        in
  let map4 f l = map (map (map (map f))) l  in

  let rblock sz id = {
    ruuid     = id;
    contents  = empty;
    opcodes   = [];
    address   = 0;
    offset    = 0;
    size      = sz; }
  in
  
  (* Byte & array manipulation convenience functions *)
  let len         = Bytes.length                                        in
  let b_tl op n   = Bytes.sub op n (len op - n)                         in
  let b_hd op n   = Bytes.sub op 0 n                                    in
  let asbtol a i  = Array.to_list (Array.sub a i (Array.length a - i))  in 

  (* Main *)
  (* Read bytes from the file, skip first 8 *) 
  let bytes = 
    let ic  = open_in Sys.argv.(binary_ind)     in 
    let len = in_channel_length ic              in
    let _   = really_input_string ic 8          in
    let res = really_input_string ic (len - 8)  in
    close_in ic; 
    res
  in

  (* Pull out interesting code bits *)
  let raw     = Runtime'.Reader.create bytes  in
  let gtirb   = IR.from_proto raw             in
  let modules = (
    match gtirb with
    | Ok ir   -> ir.modules
    | Error e -> failwith (Printf.sprintf "%s%s" "Could not reply request: " (Ocaml_protoc_plugin.Result.show_error e))
  ) in
  let all_sects = map (fun (m : Module.t) -> m.sections) modules                    in
  let all_texts = map (filter (fun (s : Section.t) -> s.name = ".text")) all_sects  in
  let intervals = map2 (fun (s : Section.t) -> s.byte_intervals) all_texts          in
  let ival_blks = map3 (fun (i : ByteInterval.t)
      -> map (fun b -> {block = b; raw = i.contents; address = i.address}) i.blocks) intervals in

  (* Resolve polymorphic block variants to isolate info we actually care about *)
  let rectify   = function
    | `Code (c : CodeBlock.t) -> rblock c.size c.uuid
    | _ -> rblock 0 empty
  in
  let poly_blks   = map4 (fun b -> {{{(rectify b.block.value)
    with offset   = b.block.offset}
    with contents = b.raw}
    with address  = b.address + b.block.offset}) ival_blks in
  
  (* only want code blocks *)
  let codes_only  = map3 (filter (fun b -> b.size > 0)) poly_blks in
  
  (* Section up byte interval contents to their respective blocks and slice out individual opcodes *)
  let trimmed   = map4 (fun b -> {b with contents = Bytes.sub b.contents b.offset b.size}) codes_only in
  let rec cut_ops contents =
    if len contents <= opcode_length then [contents]
    else ((b_hd contents opcode_length) :: cut_ops (b_tl contents opcode_length)) in
  let op_cuts   = map4 (fun b -> {b with opcodes = cut_ops b.contents}) trimmed   in

  (* Convert every opcode to big endianness *)
  let need_flip = map (fun (m : Module.t)
      -> m.byte_order = ByteOrder.LittleEndian) modules in
  let rec endian_reverse opcode = 
    if len opcode = 1 then opcode
    else cat (endian_reverse (b_tl opcode 1)) (b_hd opcode 1)                       in
  let flip_opcodes block = {block with opcodes = map endian_reverse block.opcodes}  in
  (* This could probably be optimised *)
  let rec fix_endianness flips blocks f =
    (
      match flips with
      | []      -> []
      | h :: _  -> (if h then map3 f (hd blocks) else (hd blocks))
    ) :: (
      match flips with
      | []      -> []
      | _ :: t  -> fix_endianness t (tl blocks) f
    )
  in
  let end_fixed = fix_endianness need_flip op_cuts flip_opcodes                                   in
  let blk_orded = fix_endianness need_flip end_fixed (fun b -> {b with opcodes = rev b.opcodes})  in

  (* Organise specs to allow for ASLi evaluation environment setup *)
  let prel    = Sys.argv.(prelude_ind)                                in
  let specs   = asbtol Sys.argv specs_start                           in
  let prelude = LoadASL.read_file prel true false                     in
  let mra     = map (fun t -> LoadASL.read_file t false false) specs  in
  let envinfo = concat (prelude :: mra)                               in

  (* Evaluate each opcode one by one with a new environment for each *)
  let to_asli op addr =
    let address = Some (string_of_int addr)                 in
    let env     = Eval.build_evaluation_environment envinfo in
    let str     = hex ^ Hexstring.encode op                 in 
    let res     = Dis.retrieveDisassembly ?address env str  in
    let ascii   = map Asl_utils.pp_stmt res                 in
    let rec no_newlines ascii =
      if String.length ascii = 1
      then ascii
      else (
        let s_hd s  = String.sub s 0 1                      in
        let s_tl s  = String.sub s 1 (String.length s - 1)  in
        let top     = s_hd ascii                            in
        let tail    = s_tl ascii                            in
        if top = newline
        then space ^ no_newlines tail
        else top ^ no_newlines tail
      )
    in
    let trimmed = map String.trim ascii                     in
    let joined  = map no_newlines trimmed                   in
    map (fun s -> strung ^ s ^ strung) joined
  in
  let rec asts opcodes addr envinfo =
    match opcodes with
    | []      -> []
    | h :: t  -> (to_asli h addr) :: (asts t (addr + opcode_length) envinfo)
  in
  let with_asts = map4 (fun b -> {auuid = b.ruuid; asts = (asts b.opcodes b.address envinfo); concat = ""}) blk_orded in

  (* Now massage asli outputs into a format which can be serialised and then deserialised by other tools *)
  let l_to_s op d cl l = op ^ (String.concat d l) ^ cl                        in
  let jsoned asts = map (l_to_s l_op l_dl l_cl) asts |> l_to_s l_op l_dl l_cl in
  let json_asts   = map4 (fun b -> {b with concat = jsoned b.asts}) with_asts in

  iter (iter (iter (iter (fun b -> print_endline b.concat)))) json_asts