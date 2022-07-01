include Construction_types
open Utils
open Core
module StrTable = Hashtbl.Make (String)

let values : t StrTable.t = StrTable.create ()
let names_to_ids = StrTable.create ()
let by_name_exn s = Utils.by_name_exn values names_to_ids s
let by_name s = Utils.by_name values names_to_ids s
let by_id_exn s = Utils.by_id_exn values s
let by_id s = Utils.by_id values s

let init () =
  load_iter_data "construction.json" (fun _ v ->
      let id = get_string "id" v in
      let name = get_string "name" v in
      let buildPoints = get_int "buildPoints" v in
      let buildPower = get_int "buildPower" v in
      let constructPoints = get_int "constructPoints" v in
      let hitpoints = get_int "hitpoints" v in
      let weight = get_int "weight" v in
      let droidType = Droid.DROID_CONSTRUCT in
      StrTable.add_exn
        values
        ~key:id
        ~data:{ buildPoints; buildPower; constructPoints; droidType; hitpoints; id; name; weight })
;;
