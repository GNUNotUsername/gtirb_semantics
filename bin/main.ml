open Yojson.Basic.Util;;
open Option;;
open List;;

(*
To be used later

type block =
| Code
| Data;;

type instruction_block = {
  uuid: string;
  nops: int;
  ops : string list;
  sems: string list;
  type: block;;
}
*)

(* Yojson's interpretation of a list is cooked so this terribleness is necessary *)
let to_r_list jarray =
  let rec jlen j ind =
    try
      if (index ind j |> some |> to_list |> length) = 1 then
        jlen j (ind + 1)
      else ind
    with Undefined(_, _) -> ind (* This can't be right? *)
  in let rec aux j ind max = 
    if ind < max then
      (index ind j) :: aux j (ind + 1) max
    else []
  in aux jarray 0 (jlen jarray 0);;

let sections modul = member "sections" modul |> to_r_list;;
let is_text sect = (member "name" sect |> to_string) = ".text";;
(*let text_section sects_rl = filter is_text sects_rl;;*)

let flat ll =
  match ll with
  | [] -> []
  | h :: t -> h @ (flatten t);;

(* This is dumb. Fix later *)
let map_2d f ll = map (map f) ll;;
(*let map_3d f lll = map (map (map f)) lll;;*)

let need_reverse modul = (member "byteOrder" modul |> to_string) = "LittleEndian";;

let machine_codes_b64 ts = 
  let byte_ivals = member "byteIntervals" ts |> to_r_list in
  map (member "contents") byte_ivals |> map to_string;;

let b64_to_bin b = Base64.decode_exn b |> Bytes.of_string;;

let rec instructions r =
  let l = String.length r in
  if l <= 8 then [r]
  else instructions (String.sub r 0 8) @ instructions (String.sub r 8 (l - 8));;

let rec flips mods reverses =
  let rec reverse opcode = 
    let l = String.length opcode in
    if l < 3 then opcode
    else (reverse (String.sub opcode 2 (l - 2))) ^ (String.sub opcode 0 2)
  in match reverses with
  | []      -> mods
  | h :: t  -> (match mods with
                | [] -> []
                | e :: a -> if h then (map_2d reverse e) :: (flips a t)
                            else e :: a
              );;

let () = 

  (* Load up the IR and retrieve the raw machine codes in b64 *)
  let gtirb     = try Yojson.Basic.from_file Sys.argv.(1) with Invalid_argument(_) -> exit 1  in
  let modules   = member "modules" gtirb |> to_r_list                                         in
  let all_sects = map sections modules |> flat                                                in
  let texts     = filter is_text all_sects                                                    in
  let mcodes64  = map machine_codes_b64 texts                                                 in

  (* Turn that into hex and chop out opcodes*)
  let mcs_bin   = map_2d b64_to_bin mcodes64                                                  in
  let mcs_hex   = map_2d (Hexstring.encode) mcs_bin |> map_2d String.lowercase_ascii          in
  let opcodes   = map_2d instructions mcs_hex                                                 in

  (* Correct endianness where requried *)
  let reverses  = map need_reverse modules                                                    in
  let end_fixed = flips opcodes reverses                                                      in
  iter (iter (iter print_endline)) end_fixed

  (* Give opcodes to ASLi to do all the heavy lifting *)