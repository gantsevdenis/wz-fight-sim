type upgrade_class =
  | CLS_BODY
  | CLS_BRAIN
  | CLS_BUILDING
  | CLS_CONSTRUCT
  | CLS_SENSOR
  | CLS_WEAPON

let upgrade_class_of_string s =
  match s with
  | "CLS_BODY" -> CLS_BODY
  | "CLS_BRAIN" -> CLS_BRAIN
  | "CLS_BUILDING" -> CLS_BUILDING
  | "CLS_CONSTRUCT" -> CLS_CONSTRUCT
  | "CLS_SENSOR" -> CLS_SENSOR
  | "CLS_WEAPON" -> CLS_WEAPON
  | s -> raise (Invalid_argument s)
;;

let class_to_string u =
  match u with
  | CLS_BODY -> "CLS_BODY"
  | CLS_BRAIN -> "CLS_BRAIN"
  | CLS_BUILDING -> "CLS_BUILDING"
  | CLS_CONSTRUCT -> "CLS_CONSTRUCT"
  | CLS_SENSOR -> "CLS_SENSOR"
  | CLS_WEAPON -> "CLS_WEAPON"
;;

type filter_param =
  | FP_BODYCLASS
  | FP_ID
  | FP_IMPACTCLASS
  | FP_TYPE

let filter_param_of_string s =
  match s with
  | "FP_BODYCLASS" -> FP_BODYCLASS
  | "FP_ID" -> FP_ID
  | "FP_IMPACTCLASS" -> FP_IMPACTCLASS
  | "FP_TYPE" -> FP_TYPE
  | s -> raise (Invalid_argument s)
;;

let fparam_to_string u =
  match u with
  | FP_BODYCLASS -> "FP_BODYCLASS"
  | FP_ID -> "FP_ID"
  | FP_IMPACTCLASS -> "FP_IMPACTCLASS"
  | FP_TYPE -> "FP_TYPE"
;;

type filter_value =
  | FV_AA_GUN
  | FV_BOMB
  | FV_CANNON
  | FV_COMMANDBRAIN01
  | FV_CYBORGS
  | FV_DROIDS
  | FV_ENERGY
  | FV_FLAME
  | FV_GAUSS
  | FV_HOWITZERS
  | FV_MACHINE
  | FV_MISSILE
  | FV_MORTARS
  | FV_ROCKET
  | FV_STRUCTURE
  | FV_WALL

let filter_value_of_string s =
  match s with
  | "FV_AA_GUN" -> FV_AA_GUN
  | "FV_BOMB" -> FV_BOMB
  | "FV_CANNON" -> FV_CANNON
  | "FV_COMMANDBRAIN01" -> FV_COMMANDBRAIN01
  | "FV_CYBORGS" -> FV_CYBORGS
  | "FV_DROIDS" -> FV_DROIDS
  | "FV_ENERGY" -> FV_ENERGY
  | "FV_FLAME" -> FV_FLAME
  | "FV_GAUSS" -> FV_GAUSS
  | "FV_HOWITZERS" -> FV_HOWITZERS
  | "FV_MACHINE" -> FV_MACHINE
  | "FV_MISSILE" -> FV_MISSILE
  | "FV_MORTARS" -> FV_MORTARS
  | "FV_ROCKET" -> FV_ROCKET
  | "FV_STRUCTURE" -> FV_STRUCTURE
  | "FV_WALL" -> FV_WALL
  | s -> raise (Invalid_argument s)
;;

let fvalue_to_string u =
  match u with
  | FV_AA_GUN -> "FV_AA_GUN"
  | FV_BOMB -> "FV_BOMB"
  | FV_CANNON -> "FV_CANNON"
  | FV_COMMANDBRAIN01 -> "FV_COMMANDBRAIN01"
  | FV_CYBORGS -> "FV_CYBORGS"
  | FV_DROIDS -> "FV_DROIDS"
  | FV_ENERGY -> "FV_ENERGY"
  | FV_FLAME -> "FV_FLAME"
  | FV_GAUSS -> "FV_GAUSS"
  | FV_HOWITZERS -> "FV_HOWITZERS"
  | FV_MACHINE -> "FV_MACHINE"
  | FV_MISSILE -> "FV_MISSILE"
  | FV_MORTARS -> "FV_MORTARS"
  | FV_ROCKET -> "FV_ROCKET"
  | FV_STRUCTURE -> "FV_STRUCTURE"
  | FV_WALL -> "FV_WALL"
;;

type upgrade_parameter =
  | P_ARMOUR
  | P_BASE_COMMAND_LIMIT
  | P_COMMAND_LIMIT_BY_LEVEL
  | P_CONSTRUCTOR_POINTS
  | P_DAMAGE
  | P_FIRE_PAUSE
  | P_HIT_CHANCE
  | P_HIT_POINT_PCT
  | P_HIT_POINTS
  | P_POWER
  | P_POWER_POINTS
  | P_PRODUCTION_POINTS
  | P_RADIUS_DAMAGE
  | P_RANGE
  | P_RANK_THRESHOLDS
  | P_REARM_POINTS
  | P_RELOAD_TIME
  | P_REPAIR_POINTS
  | P_REPEAT_DAMAGE (* periodical damage upgrade *)
  | P_RESEARCH_POINTS
  | P_RESISTANCE
  | P_SHORT_HIT_CHANCE
  | P_THERMAL

let upgrade_parameter_of_string s =
  match s with
  | "P_ARMOUR" -> P_ARMOUR
  | "P_BASE_COMMAND_LIMIT" -> P_BASE_COMMAND_LIMIT
  | "P_COMMAND_LIMIT_BY_LEVEL" -> P_COMMAND_LIMIT_BY_LEVEL
  | "P_CONSTRUCTOR_POINTS" -> P_CONSTRUCTOR_POINTS
  | "P_DAMAGE" -> P_DAMAGE
  | "P_FIRE_PAUSE" -> P_FIRE_PAUSE
  | "P_HIT_CHANCE" -> P_HIT_CHANCE
  | "P_HIT_POINT_PCT" -> P_HIT_POINT_PCT
  | "P_HIT_POINTS" -> P_HIT_POINTS
  | "P_POWER" -> P_POWER
  | "P_POWER_POINTS" -> P_POWER_POINTS
  | "P_PRODUCTION_POINTS" -> P_PRODUCTION_POINTS
  | "P_RADIUS_DAMAGE" -> P_RADIUS_DAMAGE
  | "P_RANGE" -> P_RANGE
  | "P_RANK_THRESHOLDS" -> P_RANK_THRESHOLDS
  | "P_REARM_POINTS" -> P_REARM_POINTS
  | "P_RELOAD_TIME" -> P_RELOAD_TIME
  | "P_REPAIR_POINTS" -> P_REPAIR_POINTS
  | "P_REPEAT_DAMAGE" -> P_REPEAT_DAMAGE
  | "P_RESEARCH_POINTS" -> P_RESEARCH_POINTS
  | "P_RESISTANCE" -> P_RESISTANCE
  | "P_SHORT_HIT_CHANCE" -> P_SHORT_HIT_CHANCE
  | "P_THERMAL" -> P_THERMAL
  | s -> raise (Invalid_argument s)
;;

let param_to_string u =
  match u with
  | P_ARMOUR -> "P_ARMOUR"
  | P_BASE_COMMAND_LIMIT -> "P_BASE_COMMAND_LIMIT"
  | P_COMMAND_LIMIT_BY_LEVEL -> "P_COMMAND_LIMIT_BY_LEVEL"
  | P_CONSTRUCTOR_POINTS -> "P_CONSTRUCTOR_POINTS"
  | P_DAMAGE -> "P_DAMAGE"
  | P_FIRE_PAUSE -> "P_FIRE_PAUSE"
  | P_HIT_CHANCE -> "P_HIT_CHANCE"
  | P_HIT_POINT_PCT -> "P_HIT_POINT_PCT"
  | P_HIT_POINTS -> "P_HIT_POINTS"
  | P_POWER -> "P_POWER"
  | P_POWER_POINTS -> "P_POWER_POINTS"
  | P_PRODUCTION_POINTS -> "P_PRODUCTION_POINTS"
  | P_RADIUS_DAMAGE -> "P_RADIUS_DAMAGE"
  | P_RANGE -> "P_RANGE"
  | P_RANK_THRESHOLDS -> "P_RANK_THRESHOLDS"
  | P_REARM_POINTS -> "P_REARM_POINTS"
  | P_RELOAD_TIME -> "P_RELOAD_TIME"
  | P_REPAIR_POINTS -> "P_REPAIR_POINTS"
  | P_REPEAT_DAMAGE -> "P_REPEAT_DAMAGE"
  | P_RESEARCH_POINTS -> "P_RESEARCH_POINTS"
  | P_RESISTANCE -> "P_RESISTANCE"
  | P_SHORT_HIT_CHANCE -> "P_SHORT_HIT_CHANCE"
  | P_THERMAL -> "P_THERMAL"
;;

type filter = filter_param * filter_value
type t = Upgrade of upgrade_class * upgrade_parameter * filter option * Pct.t

let to_string u =
  match u with
  | Upgrade (cls, param, None, pct) ->
    Printf.sprintf "(%s %s %i)" (class_to_string cls) (param_to_string param) (Pct.scale_by pct 100)
  | Upgrade (cls, param, Some (fp, fv), pct) ->
    let f = Printf.sprintf "%s %s" (fparam_to_string fp) (fvalue_to_string fv) in
    Printf.sprintf "(%s %s <%s> %i)" (class_to_string cls) (param_to_string param) f (Pct.scale_by pct 100)
;;
