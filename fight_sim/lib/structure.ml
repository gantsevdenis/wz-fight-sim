include Structure_types
open Core
open Utils
module StrTable = Hashtbl.Make (String)

let values : t StrTable.t = StrTable.create ()
let names_to_ids = StrTable.create ()
let by_name_exn s = Utils.by_name_exn values names_to_ids s
let by_name s = Utils.by_name values names_to_ids s
let by_id_exn s = Utils.by_id_exn values s
let by_id s = Utils.by_id values s

let calc_dmg (s : t) (w : Weapon_types.t) =
  assert (w.damage > 0);
  let modif = dmg_modifier w s.strength in
  max (Pct.scale_by modif w.damage) 1
;;

let conv_struct_type s =
  match s with
  | "REARM PAD" -> "REARM_PAD"
  | "MISSILE SILO" -> "MISSILE_SILO"
  | "COMMAND RELAY" -> "COMMAND"
  | "CORNER WALL" -> "CORNER"
  | "CYBORG FACTORY" -> "CYBORG"
  | "FACTORY MODULE" -> "FACTORY_MODULE"
  | "POWER GENERATOR" -> "POWER_GENERATOR"
  | "POWER MODULE" -> "POWER_MODULE"
  | "REPAIR FACILITY" -> "REPAIR_FACILITY"
  | "RESEARCH MODULE" -> "RESEARCH_MODULE"
  | "RESOURCE EXTRACTOR" -> "RESOURCE_EXTRACTOR"
  | "SAT UPLINK" -> "SAT"
  | "VTOL FACTORY" -> "VTOL"
  | "WALL" -> "WALL"
  | "DEFENSE" -> "DEFENSE"
  | "DEMOLISH" -> "DEMOLISH"
  | "FACTORY" -> "FACTORY"
  | "GATE" -> "GATE"
  | "GENERIC" -> "GENERIC"
  | "HQ" -> "HQ"
  | "LASSAT" -> "LASSAT"
  | "REARM" -> "REARM"
  | "RESEARCH" -> "RESEARCH"
  | _ -> assert false
;;

let init () =
  assert (Hashtbl.length values = 0);
  load_iter_data "structure.json" (fun k v ->
      let heat = get_int "thermal" v in
      let kinetic = get_int "armour" v in
      let armour x =
        match x with
        (* avoid cyclic dependency *)
        | Weapon_types.KINETIC -> kinetic
        | Weapon_types.HEAT -> heat
      in
      let breadth = get_int "breadth" v in
      let buildPoints = get_int "buildPoints" v in
      let buildPower = get_int "buildPower" v in
      let height = get_int ~d:1 "height" v in
      let hitPoints = get_int ~d:1 "hitPoints" v in
      let name = get_string "name" v in
      let resistance = get_int "resistance" v in
      let typ = struct_type_of_string (conv_struct_type (get_string "type" v)) in
      let strength = struct_strength_of_string (get_string "strength" v) in
      let width = get_int ~d:1 "width" v in
      let sensorId = U.member "sensorID" v |> U.to_string_option in
      Dolog.Log.debug "adding structure '%s' - '%s'" k name;
      StrTable.add_exn names_to_ids ~key:name ~data:k;
      StrTable.add_exn
        values
        ~key:k
        ~data:
          { id = k
          ; armour
          ; breadth
          ; buildPoints
          ; buildPower
          ; height
          ; hitPoints
          ; name
          ; resistance
          ; typ
          ; strength
          ; sensorId
          ; width
          })
;;
(* ignore (Structure_types.by_id_exn "X-Super-Rocket") *)
