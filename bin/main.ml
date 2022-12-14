open Yojson.Basic.Util;;
open Option;;
open List;;

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

let machine_codes_b64 ts = 
  let byte_ivals = member "byteIntervals" ts |> to_r_list in
  map (member "contents") byte_ivals |> map to_string;;

(* Load up the IR and dump out the raw machine codes in b64 *)
let gtirb     = try Yojson.Basic.from_file Sys.argv.(1) with Invalid_argument(_) -> exit 1;;
let modules   = member "modules" gtirb;;
let mods_rl   = to_r_list modules;;
let all_sects = map sections mods_rl |> flat;;
let texts     = filter is_text all_sects;;
let mcodes64  = map machine_codes_b64 texts;;
iter (iter print_endline) (mcodes64);;

(* Turn that into hex *)

(* Determine each module's endianness *)