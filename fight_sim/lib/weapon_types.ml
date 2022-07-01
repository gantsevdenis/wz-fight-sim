open Core

type weapon_effect =
  | ALL_ROUNDER
  | ANTI_PERSONNEL
  | ANTI_TANK
  | ARTILLERY_ROUND
  | BUNKER_BUSTER
  | FLAMER

type weapon_class =
  | KINETIC
  | HEAT

type weapon_subclass =
  | AA_GUN
  | BOMB
  | CANNON
  | COMMAND
  | ELECTRONIC
  | EMP
  | ENERGY
  | FLAME
  | GAUSS
  | HOWITZERS
  | LAS_SAT
  | MACHINE_GUN
  | MISSILE
  | MORTARS
  | ROCKET

type weapon_movement =
  | DIRECT
  | HOMING_DIRECT
  | HOMING_INDIRECT
  | INDIRECT

type weapon_flags =
  | AIR_ONLY
  | BOTH
  | GROUND

type t =
  { buildPoints : int
  ; buildPower : int
  ; damage : int
  ; designable : bool
  ; dmgClass : weapon_class
  ; dmgSubClass : weapon_subclass
  ; effect : weapon_effect
  ; effectSize : Pct.t
  ; firePause : int
  ; fireOnMove : bool
  ; flags : weapon_flags
  ; flightSpeed : int
  ; hitpoints : int
  ; id : string
  ; longHit : Pct.t
  ; longRange : int
  ; minimumDamage : Pct.t
  ; minRange : int
  ; movement : weapon_movement
  ; name : string
  ; numAttackRuns : int
        (*  if weapon is a salvo weapon, then number of shots that can be fired = numAttackRuns * numRounds *)
  ; numExplosions : int
  ; numRounds : int
  ; penetrate : bool
  ; periodicalDmg : int
  ; periodicalDmgRadius : int
  ; periodicalDmgTime : int
  ; periodicalDmgClass : weapon_class
  ; periodicalDmgEffect : weapon_effect
  ; periodicalDmgSubClass : weapon_subclass
  ; radius : int
  ; radiusDamage : int
  ; radiusLife : int
  ; recoilValue : int
  ; reloadTime : int
  ; rotate : int
  ; shortHit : Pct.t
  ; shortRange : int
  ; weight : int
  }

type expected_t =
  { salvos : int
  ; sec : int
  }

let expected_of salvos sec = { salvos; sec }

let weapon_effect_of_string s =
  match s with
  | "ALL_ROUNDER" -> ALL_ROUNDER
  | "ANTI_PERSONNEL" -> ANTI_PERSONNEL
  | "ANTI_TANK" -> ANTI_TANK
  | "ARTILLERY_ROUND" -> ARTILLERY_ROUND
  | "BUNKER_BUSTER" -> BUNKER_BUSTER
  | "FLAMER" -> FLAMER
  | s -> raise (Invalid_argument s)
;;

let weapon_class_of_string s =
  match s with
  | "KINETIC" -> KINETIC
  | "HEAT" -> HEAT
  | s -> raise (Invalid_argument s)
;;

let weapon_subclass_of_string s =
  match s with
  | "AA_GUN" -> AA_GUN
  | "BOMB" -> BOMB
  | "CANNON" -> CANNON
  | "COMMAND" -> COMMAND
  | "ELECTRONIC" -> ELECTRONIC
  | "EMP" -> EMP
  | "ENERGY" -> ENERGY
  | "FLAME" -> FLAME
  | "GAUSS" -> GAUSS
  | "HOWITZERS" -> HOWITZERS
  | "LAS_SAT" -> LAS_SAT
  | "MACHINE_GUN" -> MACHINE_GUN
  | "MISSILE" -> MISSILE
  | "MORTARS" -> MORTARS
  | "ROCKET" -> ROCKET
  | s -> raise (Invalid_argument s)
;;

let weapon_movement_of_string s =
  match s with
  | "DIRECT" -> DIRECT
  | "HOMING_DIRECT" -> HOMING_DIRECT
  | "HOMING_INDIRECT" -> HOMING_INDIRECT
  | "INDIRECT" -> INDIRECT
  | s -> raise (Invalid_argument s)
;;

let weapon_flags_of_string s =
  match s with
  | "AIR_ONLY" -> AIR_ONLY
  | "BOTH" -> BOTH
  | "GROUND" -> GROUND
  | s -> raise (Invalid_argument s)
;;

let filter_value_of scl =
  let open Upgrades in
  match scl with
  | AA_GUN -> FV_AA_GUN
  | BOMB -> FV_BOMB
  | CANNON -> FV_CANNON
  | COMMAND -> FV_COMMANDBRAIN01
  | ELECTRONIC -> assert false (* doesn't happen *)
  | EMP -> assert false (* doesn't happen *)
  | ENERGY -> FV_ENERGY
  | FLAME -> FV_FLAME
  | GAUSS -> FV_GAUSS
  | HOWITZERS -> FV_HOWITZERS
  | LAS_SAT -> assert false (* doesn't happen *)
  | MACHINE_GUN -> FV_MACHINE
  | MISSILE -> FV_MISSILE
  | MORTARS -> FV_MORTARS
  | ROCKET -> FV_ROCKET
;;
