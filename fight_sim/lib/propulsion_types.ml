open Core

type propulsion_type =
  | WHEELED
  | TRACKED
  | LIFT
  | PROPELLOR
  | LEGGED
  | HOVER
  | HALF_TRACKED

let propulsion_type_of_string s =
  match s with
  | "WHEELED" -> WHEELED
  | "TRACKED" -> TRACKED
  | "LIFT" -> LIFT
  | "PROPELLOR" -> PROPELLOR
  | "LEGGED" -> LEGGED
  | "HOVER" -> HOVER
  | "HALF_TRACKED" -> HALF_TRACKED
  | s -> raise (Invalid_argument s)
;;

type t =
  { id : string
  ; buildPoints : Pct.t
  ; buildPower : Pct.t
  ; deceleration : int
  ; designable : bool
  ; hitpointPctOfBody : Pct.t
  ; name : string
  ; skidDeceleration : int
  ; speed : int
  ; spinAngle : int
  ; spinSpeed : int
  ; turnSpeed : int
  ; typ : propulsion_type
  ; weightPct : Pct.t
  }

let _dmg_modifier effect prop =
  let open Weapon in
  match effect, prop.typ with
  | FLAMER, HALF_TRACKED -> Pct.of_int 100
  | FLAMER, HOVER -> Pct.of_int 130
  | FLAMER, LEGGED -> Pct.of_int 130
  | FLAMER, PROPELLOR -> Pct.cent
  | FLAMER, LIFT -> Pct.of_int 25
  | FLAMER, TRACKED -> Pct.of_int 90
  | FLAMER, WHEELED -> Pct.of_int 110
  | BUNKER_BUSTER, HALF_TRACKED -> Pct.of_int 40
  | BUNKER_BUSTER, HOVER -> Pct.of_int 20
  | BUNKER_BUSTER, LEGGED -> Pct.of_int 30
  | BUNKER_BUSTER, PROPELLOR -> Pct.cent
  | BUNKER_BUSTER, LIFT -> Pct.of_int 30
  | BUNKER_BUSTER, TRACKED -> Pct.of_int 50
  | BUNKER_BUSTER, WHEELED -> Pct.of_int 30
  | ARTILLERY_ROUND, HALF_TRACKED -> Pct.of_int 65
  | ARTILLERY_ROUND, HOVER -> Pct.of_int 110
  | ARTILLERY_ROUND, LEGGED -> Pct.of_int 130
  | ARTILLERY_ROUND, PROPELLOR -> Pct.cent
  | ARTILLERY_ROUND, LIFT -> Pct.of_int 25
  | ARTILLERY_ROUND, TRACKED -> Pct.of_int 40
  | ARTILLERY_ROUND, WHEELED -> Pct.of_int 90
  | ANTI_TANK, HALF_TRACKED -> Pct.of_int 125
  | ANTI_TANK, HOVER -> Pct.of_int 90
  | ANTI_TANK, LEGGED -> Pct.of_int 30
  | ANTI_TANK, PROPELLOR -> Pct.cent
  | ANTI_TANK, LIFT -> Pct.of_int 80
  | ANTI_TANK, TRACKED -> Pct.of_int 120
  | ANTI_TANK, WHEELED -> Pct.of_int 130
  | ANTI_PERSONNEL, HALF_TRACKED -> Pct.of_int 50
  | ANTI_PERSONNEL, HOVER -> Pct.of_int 110
  | ANTI_PERSONNEL, LEGGED -> Pct.of_int 150
  | ANTI_PERSONNEL, PROPELLOR -> Pct.cent
  | ANTI_PERSONNEL, LIFT -> Pct.of_int 60
  | ANTI_PERSONNEL, TRACKED -> Pct.of_int 40
  | ANTI_PERSONNEL, WHEELED -> Pct.of_int 100
  | ALL_ROUNDER, HALF_TRACKED -> Pct.of_int 115
  | ALL_ROUNDER, HOVER -> Pct.of_int 120
  | ALL_ROUNDER, LEGGED -> Pct.of_int 65
  | ALL_ROUNDER, PROPELLOR -> Pct.cent
  | ALL_ROUNDER, LIFT -> Pct.of_int 40
  | ALL_ROUNDER, TRACKED -> Pct.of_int 105
  | ALL_ROUNDER, WHEELED -> Pct.of_int 125
;;

let dmg_modifier (w : Weapon_types.t) (prop : t) = _dmg_modifier w.effect prop
let dmg_modifier_periodical (w : Weapon_types.t) (prop : t) = _dmg_modifier w.periodicalDmgEffect prop