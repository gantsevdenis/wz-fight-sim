include Propulsion_types
open Utils
open Core
module StrTable = Hashtbl.Make (String)

let values : t StrTable.t = StrTable.create ()

let names_to_ids = StrTable.create ()

let by_name_exn s = Utils.by_name_exn values names_to_ids s

let by_name s = Utils.by_name values names_to_ids s

let by_id_exn s = Utils.by_id_exn values s

let by_id s = Utils.by_id values s

let find_similar s =
  Utils.find_similar ~values ~names_to_ids ~by_id_exn ~by_name_exn s

let find_all_similar s =
  Utils.find_all_similar ~values ~names_to_ids ~by_id_exn ~by_name_exn s

let conv_prop_type s =
  match s with
  | "Wheeled" ->
      "WHEELED"
  | "Tracked" ->
      "TRACKED"
  | "Lift" ->
      "LIFT"
  | "Propellor" ->
      "PROPELLOR"
  | "Legged" ->
      "LEGGED"
  | "Hover" ->
      "HOVER"
  | "Half-Tracked" ->
      "HALF_TRACKED"
  | _ ->
      assert false

let init () =
  load_iter_data "propulsion.json" (fun _ v ->
      let buildPoints = Pct.of_int (get_int "buildPoints" v) in
      let buildPower = Pct.of_int (get_int "buildPower" v) in
      let deceleration = get_int ~d:800 "deceleration" v in
      let designable = get_int "designable" v = 1 in
      let hitpointPctOfBody = Pct.of_int (get_int "hitpointPctOfBody" v) in
      let name = get_string "name" v in
      let id = get_string "id" v in
      let skidDeceleration = get_int ~d:600 "skidDeceleration" v in
      let speed = get_int "speed" v in
      let spinAngle = get_int ~d:180 "spinAngle" v in
      let spinSpeed = get_int ~d:136 "spinSpeed" v in
      let turnSpeed = get_int ~d:60 "turnSpeed" v in
      let typ =
        propulsion_type_of_string (conv_prop_type (get_string "type" v))
      in
      let weightPct = Pct.of_int (get_int "weight" v) in
      Dolog.Log.debug "adding propulsion '%s' - '%s'" id name ;
      Hashtbl.add_exn names_to_ids ~key:name ~data:id ;
      Hashtbl.add_exn values ~key:id
        ~data:
          { id
          ; buildPoints
          ; buildPower
          ; deceleration
          ; designable
          ; hitpointPctOfBody
          ; name
          ; skidDeceleration
          ; speed
          ; spinAngle
          ; spinSpeed
          ; turnSpeed
          ; typ: propulsion_type
          ; weightPct } )
