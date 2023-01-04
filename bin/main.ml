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
  
  (* Decode the person and Pretty-print it *)
  let b = Runtime'.Reader.create bytes in
  let e = Gtirb_semantics.IR.Gtirb.Proto.IR.from_proto b in
  let r = (match e with
  | Ok v -> v
  | Error e -> failwith (Printf.sprintf "Could not reply request: %s" (Ocaml_protoc_plugin.Result.show_error e))) in
  let rec loop (e :  Gtirb_semantics.CFG.Gtirb.Proto.Edge.t list) = match e with
  | x::xs -> Printf.printf "%s\n" (Bytes.to_string x.source_uuid) ; loop xs
  | [] -> () in
  match r.cfg with
  | Some c -> loop c.edges
  | None -> ()
