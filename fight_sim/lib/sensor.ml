open Core
open Utils
include Sensor_types
module StrTable = Hashtbl.Make (String)

let values : t StrTable.t = StrTable.create ()
let names_to_ids = StrTable.create ()
let by_name_exn s = Utils.by_name_exn values names_to_ids s
let by_name s = Utils.by_name values names_to_ids s
let by_id_exn s = Utils.by_id_exn values s
let by_id s = Utils.by_id values s

let init () =
  assert (Hashtbl.length values = 0);
  load_iter_data "sensor.json" (fun _ v ->
      if not (String.equal (get_string "droidType" v) "")
      then (
        let id = get_string "id" v in
        let name = get_string "name" v in
        let buildPoints = get_int "buildPoints" v in
        let buildPower = get_int "buildPower" v in
        let constructPoints = get_int "constructPoints" v in
        let designable = get_int ~d:0 "designable" v = 1 in
        let hitpoints = get_int ~d:1 "hitpoints" v in
        let power = get_int "power" v in
        let range = get_int "range" v in
        let typ = sensor_type_of_string (sanitize (get_string "type" v)) in
        let weight = get_int "weight" v in
        let droidType = Droid.DROID_CONSTRUCT in
        StrTable.add_exn
          values
          ~key:id
          ~data:
            { buildPoints
            ; buildPower
            ; constructPoints
            ; designable
            ; droidType
            ; hitpoints
            ; id
            ; name
            ; power
            ; range
            ; typ
            ; weight
            }))
;;
