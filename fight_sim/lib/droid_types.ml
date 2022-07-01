type droid_type =
  | DROID_WEAPON
  | DROID_SENSOR
  | DROID_ECM
  | DROID_CONSTRUCT
  | DROID_PERSON
  | DROID_CYBORG
  | DROID_TRANSPORTER
  | DROID_COMMAND
  | DROID_REPAIR
  | DROID_DEFAULT
  | DROID_CYBORG_CONSTRUCT
  | DROID_CYBORG_REPAIR
  | DROID_CYBORG_SUPER
  | DROID_SUPERTRANSPORTER

type t =
  { template : Droid_template.t
  ; exp : int
  ; position : int * int
  ; mutable cur_hp : int
  ; purpose : droid_type
  ; has_commander : t option
  }