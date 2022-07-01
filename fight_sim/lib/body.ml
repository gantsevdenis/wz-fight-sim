open Core
include Body_types
module StrTable = Hashtbl.Make (String)

(* let values = StrTable.create ()
   let names_to_ids = StrTable.create () *)
let conv_body_clazz s = String.uppercase s

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

let init () =
  let open Utils in
  assert (Hashtbl.length values = 0) ;
  Dolog.Log.debug "loading body.json" ;
  load_iter_data "body.json" (fun _ v ->
      let name = get_string "name" v in
      if Hashtbl.mem names_to_ids name then
        Dolog.Log.debug "ignoring body with duplicate name '%s'" name
      else
        let id = get_string "id" v in
        if Hashtbl.mem values id then
          Dolog.Log.debug "ignoring body with duplicate ID '%s'" id
        else
          let heat = get_int "armourHeat" v in
          let kinetic = get_int "armourKinetic" v in
          let armour x =
            match x with
            (* don't use Weapon here, to avoid cyclic dependency *)
            | Weapon_types.KINETIC ->
                kinetic
            | Weapon_types.HEAT ->
                heat
          in
          let buildPoints = get_int "buildPoints" v in
          let buildPower = get_int "buildPower" v in
          let clazz =
            body_class_of_string
              (conv_body_clazz (get_string ~d:"DROIDS" "class" v))
          in
          let designable = get_int ~d:0 "designable" v = 1 in
          let hitpoints = get_int ~d:1 "hitpoints" v in
          let powerOutput = get_int "powerOutput" v in
          let size = body_size_of_string (sanitize (get_string "size" v)) in
          let weaponSlots = get_int ~d:1 "weaponSlots" v in
          let weight = get_int ~d:0 "weight" v in
          Dolog.Log.debug "adding body '%s' - '%s'" id name ;
          Hashtbl.add_exn names_to_ids ~key:name ~data:id ;
          Hashtbl.add_exn values ~key:id
            ~data:
              { armour
              ; name
              ; id
              ; buildPower
              ; buildPoints
              ; clazz
              ; designable
              ; hitpoints
              ; powerOutput
              ; size
              ; weaponSlots
              ; weight } )
