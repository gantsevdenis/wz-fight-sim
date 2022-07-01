type upgrade_class =
  | CLS_BODY
  | CLS_BRAIN
  | CLS_BUILDING
  | CLS_CONSTRUCT
  | CLS_SENSOR
  | CLS_WEAPON

type filter_param =
  | FP_BODYCLASS
  | FP_ID
  | FP_IMPACTCLASS
  | FP_TYPE

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

val fvalue_to_string : filter_value -> string

type upgrade_parameter =
  | P_ARMOUR (* kinetic *)
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
  | P_REPEAT_DAMAGE
  | P_RESEARCH_POINTS
  | P_RESISTANCE
  | P_SHORT_HIT_CHANCE
  | P_THERMAL

val class_to_string : upgrade_class -> string
val upgrade_class_of_string : string -> upgrade_class
val fparam_to_string : filter_param -> string
val filter_param_of_string : string -> filter_param
val param_to_string : upgrade_parameter -> string
val upgrade_parameter_of_string : string -> upgrade_parameter
val filter_value_of_string : string -> filter_value

type filter = filter_param * filter_value
type t = Upgrade of upgrade_class * upgrade_parameter * filter option * Pct.t

val to_string : t -> string
