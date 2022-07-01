open Core

type sensor_type =
  | STANDARD
  | SUPER
  | RADAR_DETECTOR
  | INDIRECT_CB
  | VTOL_CB
  | VTOL_INTERCEPT

let sensor_type_of_string s =
  match s with
  | "STANDARD" -> STANDARD
  | "SUPER" -> SUPER
  | "RADAR_DETECTOR" -> RADAR_DETECTOR
  | "INDIRECT_CB" -> INDIRECT_CB
  | "VTOL_CB" -> VTOL_CB
  | "VTOL_INTERCEPT" -> VTOL_INTERCEPT
  | _ -> raise (Invalid_argument s)
;;

type t =
  { buildPoints : int
  ; buildPower : int
  ; constructPoints : int
  ; designable : bool
  ; droidType : Droid.droid_type
  ; hitpoints : int
  ; id : string
  ; name : string
  ; power : int
  ; range : int
  ; typ : sensor_type
  ; weight : int
  }
