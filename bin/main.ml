open Ocaml_protoc_plugin.Runtime
open List

type rectified_block = {
  uuid    : bytes;
  size    : int;
  offset  : int;
}

let () = 

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

  let raw     = Runtime'.Reader.create bytes                      in
  let gtirb   = Gtirb_semantics.IR.Gtirb.Proto.IR.from_proto raw  in
  let result  = (
    match gtirb with
    | Ok ir   -> ir.modules
    | Error e -> failwith (Printf.sprintf "%s%s" death (Ocaml_protoc_plugin.Result.show_error e))
  ) in
  (* there's gotta be a better way to do this... blame the lack of functional programming experience... *)
  let all_sects = map (fun (m : Gtirb_semantics.Module.Gtirb.Proto.Module.t) -> m.sections) result                            in
  let all_texts = map (filter (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.name = ".text")) all_sects        in
  let intervals = map (map (fun (s : Gtirb_semantics.Section.Gtirb.Proto.Section.t) -> s.byte_intervals)) all_texts           in (* 2D list of all byte intervals *)
  let contents  = map (map (map (fun (i : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t) -> i.contents))) intervals in
  let ival_blks = map (map (map (fun (i : Gtirb_semantics.ByteInterval.Gtirb.Proto.ByteInterval.t) -> i.blocks))) intervals   in
  let offsets   = map (map (map (map (fun (b : Gtirb_semantics.ByteInterval.Gtirb.Proto.Block.t) -> b.offset)))) ival_blks    in
  let poly_blks = map (map (map (map (fun (b : Gtirb_semantics.ByteInterval.Gtirb.Proto.Block.t) -> b.value))))  ival_blks    in
  let rectify   = function
    | `Code (c : Gtirb_semantics.CodeBlock.Gtirb.Proto.CodeBlock.t) -> {uuid = c.uuid; size = c.size; offset = 0}
    | `Data (d : Gtirb_semantics.DataBlock.Gtirb.Proto.DataBlock.t) -> {uuid = d.uuid; size = d.size; offset = 0}
    | _ -> {uuid = Bytes.empty; size = 0; offset = 0}
  in
  let r_blocks  = map (map (map (map rectify))) poly_blks in
  (* This is pain
  let add_offsets rectified offsets =
    match rectified with
    | []      -> []
    | h::t    -> (match offsets with
                  | [] -> []
                  | g :: s -> (record_zip g h) :: )
    | [h::t]  ->
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA*)