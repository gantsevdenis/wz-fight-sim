(* type ('a, 'b) table = { contents : ('a, 'b) Core.Hashtbl.t } *)
open Core

let idiv a b = (a / b) + if a mod b > 0 then 1 else 0

module U = Yojson.Safe.Util
module StringSet = Set.Make (String)
module StrTable = Hashtbl.Make (String)

let sprintf = Printf.sprintf

let find_similar ~values ~names_to_ids ~by_id_exn ~by_name_exn s =
  let ls = String.lowercase s in
  let out = ref None in
  StrTable.iter_keys values ~f:(fun k ->
      let lk = String.lowercase k in
      if String.equal lk ls then out := Some (by_id_exn k) ) ;
  match !out with
  | Some b ->
      Some b
  | None ->
      StrTable.iter_keys
        ~f:(fun k ->
          let lk = String.lowercase k in
          if String.equal lk ls then out := Some (by_name_exn k) )
        names_to_ids ;
      !out

let find_all_similar ~values ~names_to_ids ~by_id_exn ~by_name_exn s =
  let ls = String.lowercase s in
  let out = ref [] in
  StrTable.iter_keys values ~f:(fun k ->
      let lk = String.lowercase k in
      if String.is_substring ~substring:ls lk then out := by_id_exn k :: !out ) ;
  StrTable.iter_keys
    ~f:(fun k ->
      let lk = String.lowercase k in
      if String.is_substring ~substring:ls lk then out := by_name_exn k :: !out
      )
    names_to_ids ;
  !out

let load_iter_data filename f =
  let full_filename = sprintf "%s%s" Conf.confdir filename in
  Dolog.Log.debug "loading from %s" full_filename ;
  let data = Yojson.Safe.from_file full_filename in
  let data_assoc = U.to_assoc data in
  List.iter ~f:(fun (k, v) -> f k v) data_assoc

let sanitize s =
  let open Str in
  global_replace (regexp_string "-") "_" s
  |> global_replace (regexp_string " ") "_"
  |> global_replace (regexp_string "*") ""

let or_else d o = match o with Some x -> x | None -> d

let sanitize_lower s = sanitize s |> String.lowercase

let get_int ?(d = 0) s v = U.member s v |> U.to_int_option |> or_else d

let get_string ?(d = "") s v = U.member s v |> U.to_string_option |> or_else d

let get_int_opt s v = U.member s v |> U.to_int_option

let get_string_list k v =
  List.map
    ~f:(fun x -> U.to_string x)
    (match U.member k v with `Null -> [] | ll -> U.to_list ll)

let by_name_exn values names_to_ids s =
  StrTable.(find_exn values (find_exn names_to_ids s))

let by_name values names_to_ids s =
  match StrTable.(find names_to_ids s) with
  | None ->
      None
  | Some id ->
      StrTable.(find values id)

let by_id_exn values s = StrTable.(find_exn values s)

let by_id values s = StrTable.(find values s)

let set_of_string_list sl =
  let out = StrTable.create () in
  List.iter ~f:(fun x -> StrTable.set out ~key:x ~data:0) sl ;
  out

(* load spec file:
   Example:
     [key1]
     data0
     data1

     [key2]
     data0
*)
let load_spec_file (keys : string list) (filename : string) =
  let spec = StrTable.create () in
  let cumulators = StrTable.create () in
  let cumulate_ line key =
    if String.is_substring_at ~pos:0 line ~substring:"[" then false
    else if String.is_empty line then true
    else (
      StrTable.add_multi spec ~key ~data:line ;
      true )
  in
  List.iter
    ~f:(fun k ->
      StrTable.set cumulators ~key:k ~data:(fun line -> cumulate_ line k) )
    keys ;
  let id _ = false in
  let cumulator = ref id in
  let choose line =
    match StrTable.find cumulators line with
    | None ->
        cumulator := id ;
        false
    | Some f ->
        cumulator := f ;
        true
  in
  In_channel.iter_lines (In_channel.create filename) ~f:(fun line ->
      match !cumulator line with
      | true ->
          ()
      | false ->
          if not (choose line) then raise (Invalid_argument line) ) ;
  spec
