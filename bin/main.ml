open Ocaml_protoc_plugin.Runtime

let () = 

  (* Read bytes from the file *) 
  let bytes = 
    let ic = open_in Sys.argv.(1) in 
    let len = in_channel_length ic in 
    (* Skip first 8 bytes *)
    let _ = really_input_string ic 8 in
    let res = really_input_string ic (len - 8) in
    close_in ic; 
    res
  in 
  
(* Replace with IR.Modules.Sections -> isolate .text -> text.contents *)

  let raw     = Runtime'.Reader.create bytes in
  let gtirb   = Gtirb_semantics.IR.Gtirb.Proto.IR.from_proto raw in
  let result  = (match gtirb with
  | Ok v    -> v
  | Error e -> failwith (Printf.sprintf "Could not reply request: %s" (Ocaml_protoc_plugin.Result.show_error e)))
  in
  let rec loop (e :  Gtirb_semantics.CFG.Gtirb.Proto.Edge.t list) = match e with
  | h :: t  -> Printf.printf "%s\n" (Bytes.to_string h.source_uuid) ; loop t
  | []      -> ()
  in
  match result.cfg with
  | Some graph  -> loop graph.edges
  | None        -> ()
